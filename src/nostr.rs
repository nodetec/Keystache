use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Debug;
use std::str::FromStr;
use std::time::Duration;

use fedimint_core::config::FederationId;
use fedimint_core::invite_code::InviteCode;
use iced::Subscription;
use nostr_relay_pool::RelayStatus;
use nostr_sdk::{Alphabet, EventSource, Filter, Kind, PublicKey, SingleLetterTag, TagKind, Url};

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct NostrState {
    pub relay_connections: BTreeMap<Url, RelayStatus>,
}

#[derive(Debug, Clone)]
pub enum NostrModuleMessage {
    ConnectToRelay(String),
    DisconnectFromRelay(String),
}

#[derive(Clone)]
pub struct NostrModule {
    client: nostr_sdk::Client,
}

impl NostrModule {
    pub fn new() -> Self {
        Self {
            client: nostr_sdk::Client::default(),
        }
    }

    pub fn update(&self, message: NostrModuleMessage) {
        match message {
            NostrModuleMessage::ConnectToRelay(url) => {
                let client = self.client.clone();

                tokio::spawn(async move {
                    client.add_relay(&url).await.unwrap();
                    client.connect_relay(url).await.unwrap();
                });
            }
            NostrModuleMessage::DisconnectFromRelay(url) => {
                let client = self.client.clone();

                tokio::spawn(async move {
                    client.remove_relay(&url).await.unwrap();
                });
            }
        }
    }

    pub fn subscription(&self) -> Subscription<NostrState> {
        const POLL_DURATION: Duration = Duration::from_millis(200);

        let this = self.clone();

        Subscription::run_with_id(
            std::any::TypeId::of::<NostrState>(),
            // We're wrapping `stream` in a `stream!` macro to make it lazy (meaning `stream` isn't
            // created unless the outer `stream!` is actually used). This is necessary because the
            // outer `stream!` is created on every update, but will only be polled if the subscription
            // ID is new.
            async_stream::stream! {
                let mut last_state = NostrState::default();

                tokio::time::sleep(Duration::from_secs(2)).await;
                this.find_federations().await;

                loop {
                    let new_state = this.get_state().await;
                    if new_state != last_state {
                        yield new_state.clone();
                        last_state = new_state;
                    }

                    tokio::time::sleep(POLL_DURATION).await;
                }
            },
        )
    }

    pub async fn find_federations(&self) {
        println!("Finding federation recommendations...");

        let fedimint_recommendation_events = self
            .client
            .get_events_of(
                vec![Filter::new()
                    .kind(Kind::Custom(38_000))
                    .custom_tag(SingleLetterTag::lowercase(Alphabet::K), vec!["38173"])
                    .custom_tag(SingleLetterTag::lowercase(Alphabet::N), vec!["mainnet"])],
                EventSource::both(None),
            )
            .await
            .unwrap();

        let mut federations: BTreeMap<FederationId, (BTreeSet<PublicKey>, BTreeSet<InviteCode>)> =
            BTreeMap::new();
        for recommendation_event in &fedimint_recommendation_events {
            for d_tag in recommendation_event.get_tags_content(TagKind::SingleLetter(
                SingleLetterTag::lowercase(Alphabet::D),
            )) {
                if let Ok(federation_id) = FederationId::from_str(d_tag) {
                    let (recommenders, invite_codes) = federations
                        .entry(federation_id)
                        .or_insert_with(|| (BTreeSet::new(), BTreeSet::new()));

                    recommenders.insert(recommendation_event.pubkey);
                    for u_tag in recommendation_event.get_tags_content(TagKind::SingleLetter(
                        SingleLetterTag::lowercase(Alphabet::U),
                    )) {
                        if let Ok(invite_code) = InviteCode::from_str(u_tag) {
                            if invite_code.federation_id() == federation_id {
                                invite_codes.insert(invite_code);
                            }
                        }
                    }
                }
            }
        }

        println!("{:#?}", federations);
        println!(
            "Found {} recommendations for {} federations",
            fedimint_recommendation_events.len(),
            federations.len()
        );
    }

    /// Fetches the current state of the Nostr SDK client.
    /// Note: This is async because it's grabbing read locks
    /// on the relay `RwLock`s. No network requests are made.
    async fn get_state(&self) -> NostrState {
        let mut relay_connections = BTreeMap::new();

        for (url, relay) in self.client.relays().await {
            relay_connections.insert(url.clone(), relay.status().await);
        }

        NostrState { relay_connections }
    }
}
