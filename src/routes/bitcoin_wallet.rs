use std::{
    cmp::Ordering,
    collections::{BTreeMap, BTreeSet},
    str::FromStr,
};

use fedimint_core::{
    config::{ClientConfig, FederationId, META_FEDERATION_NAME_KEY},
    invite_code::InviteCode,
    Amount,
};
use iced::{
    futures::{stream::FuturesUnordered, StreamExt},
    widget::{
        column, container::Style, horizontal_space, row, text_input, Column, Container, Space, Text,
    },
    Border, Element, Length, Shadow, Task, Theme,
};
use nostr_sdk::PublicKey;

use crate::{
    app,
    fedimint::FederationView,
    ui_components::{icon_button, PaletteColor, SvgIcon},
    util::{format_amount, lighten, truncate_text},
    ConnectedState,
};

use super::{container, Loadable, RouteName};

mod receive;
mod send;

#[derive(Debug, Clone)]
pub enum Message {
    JoinFederationInviteCodeInputChanged(String),

    LoadedFederationConfigFromInviteCode {
        // The invite code that was used to load the federation config.
        config_invite_code: InviteCode,
        // The loaded federation config.
        config: ClientConfig,
    },
    FailedToLoadFederationConfigFromInviteCode {
        // The invite code that was used to attempt to load the federation config.
        config_invite_code: InviteCode,
    },

    LoadNip87Federations,
    LoadedNip87Federations(BTreeMap<FederationId, (BTreeSet<PublicKey>, BTreeSet<InviteCode>)>),
    JoinFedimintFederation(InviteCode),
    ConnectedToFederation,

    Send(send::Message),
    Receive(receive::Message),

    UpdateFederationViews(BTreeMap<FederationId, FederationView>),
}

pub struct Page {
    pub connected_state: ConnectedState,
    pub subroute: Subroute,
}

impl Page {
    // TODO: Remove this clippy allow.
    #[allow(clippy::too_many_lines)]
    pub fn update(&mut self, msg: Message) -> Task<app::Message> {
        match msg {
            Message::JoinFederationInviteCodeInputChanged(new_federation_invite_code) => {
                let Subroute::Add(Add {
                    federation_invite_code,
                    parsed_federation_invite_code_state_or,
                    ..
                }) = &mut self.subroute
                else {
                    return Task::none();
                };

                *federation_invite_code = new_federation_invite_code;

                if let Ok(invite_code) = InviteCode::from_str(federation_invite_code) {
                    *parsed_federation_invite_code_state_or =
                        Some(ParsedFederationInviteCodeState {
                            invite_code: invite_code.clone(),
                            loadable_federation_config: Loadable::Loading,
                        });

                    Task::future(async move {
                        match fedimint_api_client::download_from_invite_code(&invite_code).await {
                            Ok(config) => app::Message::Routes(super::Message::BitcoinWalletPage(
                                Message::LoadedFederationConfigFromInviteCode {
                                    config_invite_code: invite_code,
                                    config,
                                },
                            )),
                            // TODO: Include error in message and display it in the UI.
                            Err(_err) => app::Message::Routes(super::Message::BitcoinWalletPage(
                                Message::FailedToLoadFederationConfigFromInviteCode {
                                    config_invite_code: invite_code,
                                },
                            )),
                        }
                    })
                } else {
                    *parsed_federation_invite_code_state_or = None;

                    Task::none()
                }
            }
            Message::LoadedFederationConfigFromInviteCode {
                config_invite_code,
                config,
            } => {
                let Subroute::Add(Add {
                    parsed_federation_invite_code_state_or,
                    nip_87_data_or,
                    ..
                }) = &mut self.subroute
                else {
                    return Task::none();
                };

                if let Some(ParsedFederationInviteCodeState {
                    invite_code,
                    loadable_federation_config: maybe_loading_federation_config,
                }) = parsed_federation_invite_code_state_or
                {
                    // If the invite code has changed since the request was made, ignore the response.
                    if &config_invite_code == invite_code {
                        *maybe_loading_federation_config = Loadable::Loaded(config.clone());
                    }
                }

                if let Some(Loadable::Loaded(nip_87_data)) = nip_87_data_or {
                    for (_, invite_codes) in nip_87_data.values_mut() {
                        for invite_code in invite_codes {
                            if invite_code.invite_code == config_invite_code
                                && invite_code.loadable_federation_config == Loadable::Loading
                            {
                                invite_code.loadable_federation_config =
                                    Loadable::Loaded(config.clone());
                            }
                        }
                    }
                }

                Task::none()
            }
            Message::FailedToLoadFederationConfigFromInviteCode { config_invite_code } => {
                let Subroute::Add(Add {
                    parsed_federation_invite_code_state_or,
                    nip_87_data_or,
                    ..
                }) = &mut self.subroute
                else {
                    return Task::none();
                };

                if let Some(ParsedFederationInviteCodeState {
                    invite_code,
                    loadable_federation_config: maybe_loading_federation_config,
                }) = parsed_federation_invite_code_state_or
                {
                    // If the invite code has changed since the request was made, ignore the response.
                    // Also only update the state if the config hasn't already been loaded.
                    if &config_invite_code == invite_code
                        && matches!(maybe_loading_federation_config, Loadable::Loading)
                    {
                        *maybe_loading_federation_config = Loadable::Failed;
                    }
                }

                if let Some(Loadable::Loaded(nip_87_data)) = nip_87_data_or {
                    for (_, invite_codes) in nip_87_data.values_mut() {
                        for invite_code in invite_codes {
                            if invite_code.invite_code == config_invite_code
                                && invite_code.loadable_federation_config == Loadable::Loading
                            {
                                invite_code.loadable_federation_config = Loadable::Failed;
                            }
                        }
                    }
                }

                Task::none()
            }
            Message::LoadNip87Federations => {
                if let Subroute::Add(add_page) = &mut self.subroute {
                    add_page.nip_87_data_or = Some(Loadable::Loading);
                }

                let nostr_module = self.connected_state.nostr_module.clone();

                Task::future(async move {
                    match nostr_module.find_federations().await {
                        Ok(federations) => app::Message::Routes(super::Message::BitcoinWalletPage(
                            Message::LoadedNip87Federations(federations),
                        )),
                        Err(err) => {
                            panic!()
                        }
                    }
                })
            }
            Message::LoadedNip87Federations(nip_87_data) => {
                if let Subroute::Add(add_page) = &mut self.subroute {
                    // Only set the state to loaded if the user requested the data.
                    // This prevents the data from being displayed if the user
                    // navigates away from the page and back before the data is loaded.
                    if matches!(add_page.nip_87_data_or, Some(Loadable::Loading)) {
                        add_page.nip_87_data_or = Some(Loadable::Loaded(
                            nip_87_data
                                .clone()
                                .into_iter()
                                .map(|(federation_id, (pubkeys, invite_codes))| {
                                    (
                                        federation_id,
                                        (
                                            pubkeys,
                                            invite_codes
                                                .into_iter()
                                                .map(|invite_code| {
                                                    ParsedFederationInviteCodeState {
                                                        invite_code,
                                                        loadable_federation_config:
                                                            Loadable::Loading,
                                                    }
                                                })
                                                .collect(),
                                        ),
                                    )
                                })
                                .collect(),
                        ));
                    }
                }

                Task::stream(async_stream::stream! {
                    let mut futures = FuturesUnordered::new();

                    for (_, (_, invite_codes)) in nip_87_data {
                        if let Some(invite_code) = invite_codes.first().cloned() {
                            futures.push(async move {
                                match fedimint_api_client::download_from_invite_code(&invite_code).await {
                                    Ok(config) => {
                                        app::Message::Routes(super::Message::BitcoinWalletPage(
                                            Message::LoadedFederationConfigFromInviteCode {
                                                config_invite_code: invite_code.clone(),
                                                config,
                                            },
                                        ))
                                    }
                                    // TODO: Include error in message and display it in the UI.
                                    Err(_err) => {
                                        app::Message::Routes(super::Message::BitcoinWalletPage(
                                            Message::FailedToLoadFederationConfigFromInviteCode {
                                                config_invite_code: invite_code.clone(),
                                            },
                                        ))
                                    }
                                }
                            });
                        }
                    }

                    while let Some(result) = futures.next().await {
                        yield result;
                    }
                })
            }
            Message::JoinFedimintFederation(invite_code) => {
                let wallet = self.connected_state.wallet.clone();

                Task::future(async move {
                    wallet.join_federation(invite_code).await.unwrap();
                    app::Message::Routes(super::Message::BitcoinWalletPage(
                        Message::ConnectedToFederation,
                    ))
                })
            }
            Message::ConnectedToFederation => {
                // TODO: Do something here, or remove `ConnectedToFederation` message variant.

                Task::none()
            }
            Message::Send(send_message) => {
                if let Subroute::Send(send_page) = &mut self.subroute {
                    send_page.update(send_message)
                } else {
                    Task::none()
                }
            }
            Message::Receive(receive_message) => {
                if let Subroute::Receive(receive_page) = &mut self.subroute {
                    receive_page.update(receive_message)
                } else {
                    Task::none()
                }
            }
            Message::UpdateFederationViews(federation_views) => match &mut self.subroute {
                Subroute::Send(send_page) => {
                    send_page.update(send::Message::UpdateFederationViews(federation_views))
                }
                Subroute::Receive(receive_page) => {
                    receive_page.update(receive::Message::UpdateFederationViews(federation_views))
                }
                _ => Task::none(),
            },
        }
    }

    pub fn view(&self) -> Column<app::Message> {
        match &self.subroute {
            Subroute::List(list) => list.view(&self.connected_state),
            Subroute::FederationDetails(federation_details) => federation_details.view(),
            Subroute::Add(add) => add.view(&self.connected_state),
            Subroute::Send(send) => send.view(),
            Subroute::Receive(receive) => receive.view(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SubrouteName {
    List,
    FederationDetails(FederationView),
    Add,
    Send,
    Receive,
}

impl SubrouteName {
    pub fn to_default_subroute(&self, connected_state: &ConnectedState) -> Subroute {
        match self {
            Self::List => Subroute::List(List {}),
            Self::FederationDetails(federation_view) => {
                Subroute::FederationDetails(FederationDetails {
                    view: federation_view.clone(),
                })
            }
            Self::Add => Subroute::Add(Add {
                federation_invite_code: String::new(),
                parsed_federation_invite_code_state_or: None,
                nip_87_data_or: None,
            }),
            Self::Send => Subroute::Send(send::Page::new(connected_state)),
            Self::Receive => Subroute::Receive(receive::Page::new(connected_state)),
        }
    }
}

pub enum Subroute {
    List(List),
    FederationDetails(FederationDetails),
    Add(Add),
    Send(send::Page),
    Receive(receive::Page),
}

impl Subroute {
    pub fn to_name(&self) -> SubrouteName {
        match self {
            Self::List(_) => SubrouteName::List,
            Self::FederationDetails(federation_details) => {
                SubrouteName::FederationDetails(federation_details.view.clone())
            }
            Self::Add(_) => SubrouteName::Add,
            Self::Send(_) => SubrouteName::Send,
            Self::Receive(_) => SubrouteName::Receive,
        }
    }
}

pub struct List {}

impl List {
    // TODO: Remove this clippy allow.
    #[allow(clippy::unused_self)]
    fn view<'a>(&self, connected_state: &ConnectedState) -> Column<'a, app::Message> {
        let mut container = container("Wallet");

        match &connected_state.loadable_federation_views {
            Loadable::Loading => {
                container = container.push(Text::new("Loading federations...").size(25));
            }
            Loadable::Loaded(views) => {
                container = container
                    .push(
                        Text::new(format_amount(Amount::from_msats(
                            views
                                .iter()
                                .map(|(_federation_id, view)| view.balance.msats)
                                .sum::<u64>(),
                        )))
                        .size(35),
                    )
                    .push(row![
                        icon_button("Send", SvgIcon::ArrowUpward, PaletteColor::Primary).on_press(
                            app::Message::Routes(super::Message::Navigate(
                                RouteName::BitcoinWallet(SubrouteName::Send)
                            ))
                        ),
                        Space::with_width(10.0),
                        icon_button("Receive", SvgIcon::ArrowDownward, PaletteColor::Primary)
                            .on_press(app::Message::Routes(super::Message::Navigate(
                                RouteName::BitcoinWallet(SubrouteName::Receive)
                            )))
                    ])
                    .push(Text::new("Federations").size(25));

                for view in views.values() {
                    let column: Column<_, Theme, _> = Column::new()
                        .push(
                            Text::new(
                                view.name_or
                                    .clone()
                                    .unwrap_or_else(|| "Unnamed Federation".to_string()),
                            )
                            .size(25),
                        )
                        .push(Text::new(format_amount(view.balance)));

                    container = container.push(
                        Container::new(row![
                            column,
                            horizontal_space(),
                            icon_button("Details", SvgIcon::ChevronRight, PaletteColor::Background)
                                .on_press(app::Message::Routes(super::Message::Navigate(
                                    RouteName::BitcoinWallet(SubrouteName::FederationDetails(
                                        view.clone()
                                    ))
                                )))
                        ])
                        .padding(10)
                        .width(Length::Fill)
                        .style(|theme| -> Style {
                            Style {
                                text_color: None,
                                background: Some(lighten(theme.palette().background, 0.05).into()),
                                border: Border {
                                    color: iced::Color::WHITE,
                                    width: 0.0,
                                    radius: (8.0).into(),
                                },
                                shadow: Shadow::default(),
                            }
                        }),
                    );
                }
            }
            Loadable::Failed => {
                container =
                    container.push(Text::new("Failed to load federation config views.").size(25));
            }
        }

        container = container.push(
            icon_button("Join Federation", SvgIcon::Add, PaletteColor::Primary).on_press(
                app::Message::Routes(super::Message::Navigate(RouteName::BitcoinWallet(
                    SubrouteName::Add,
                ))),
            ),
        );

        container
    }
}

pub struct FederationDetails {
    view: FederationView,
}

impl FederationDetails {
    fn view<'a>(&self) -> Column<'a, app::Message> {
        let mut container = container("Federation Details")
            .push(
                Text::new(
                    self.view
                        .name_or
                        .clone()
                        .unwrap_or_else(|| "Unnamed Federation".to_string()),
                )
                .size(25),
            )
            .push(Text::new(format!(
                "Federation ID: {}",
                truncate_text(&self.view.federation_id.to_string(), 23, true)
            )))
            .push(Text::new(format_amount(self.view.balance)))
            .push(Text::new("Gateways").size(20));

        for gateway in &self.view.gateways {
            let vetted_text = if gateway.vetted {
                "Vetted"
            } else {
                "Not Vetted"
            };

            let column: Column<_, Theme, _> = column![
                Text::new(format!(
                    "Gateway ID: {}",
                    truncate_text(&gateway.info.gateway_id.to_string(), 43, true)
                )),
                Text::new(format!(
                    "Lightning Node Alias: {}",
                    truncate_text(&gateway.info.lightning_alias.to_string(), 43, true)
                )),
                Text::new(format!(
                    "Lightning Node Public Key: {}",
                    truncate_text(&gateway.info.node_pub_key.to_string(), 43, true)
                )),
                Text::new(vetted_text)
            ];

            container = container.push(
                Container::new(column)
                    .padding(10)
                    .width(Length::Fill)
                    .style(|theme| -> Style {
                        Style {
                            text_color: None,
                            background: Some(lighten(theme.palette().background, 0.05).into()),
                            border: Border {
                                color: iced::Color::WHITE,
                                width: 0.0,
                                radius: (8.0).into(),
                            },
                            shadow: Shadow::default(),
                        }
                    }),
            );
        }

        container = container.push(
            icon_button("Back", SvgIcon::ArrowBack, PaletteColor::Background).on_press(
                app::Message::Routes(super::Message::Navigate(RouteName::BitcoinWallet(
                    SubrouteName::List,
                ))),
            ),
        );

        container
    }
}

pub struct Add {
    federation_invite_code: String,
    parsed_federation_invite_code_state_or: Option<ParsedFederationInviteCodeState>,
    nip_87_data_or: Option<
        Loadable<
            BTreeMap<FederationId, (BTreeSet<PublicKey>, Vec<ParsedFederationInviteCodeState>)>,
        >,
    >,
}

#[derive(PartialEq, Eq)]
pub struct ParsedFederationInviteCodeState {
    invite_code: InviteCode,
    loadable_federation_config: Loadable<ClientConfig>,
}

impl PartialOrd for ParsedFederationInviteCodeState {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ParsedFederationInviteCodeState {
    fn cmp(&self, other: &Self) -> Ordering {
        self.invite_code.cmp(&other.invite_code)
    }
}

impl Add {
    fn view<'a>(&self, connected_state: &ConnectedState) -> Column<'a, app::Message> {
        let mut container = container("Join Federation")
            .push(
                text_input("Federation Invite Code", &self.federation_invite_code)
                    .on_input(|input| {
                        app::Message::Routes(super::Message::BitcoinWalletPage(
                            Message::JoinFederationInviteCodeInputChanged(input),
                        ))
                    })
                    .padding(10)
                    .size(30),
            )
            .push(
                icon_button("Join Federation", SvgIcon::Groups, PaletteColor::Primary)
                    .on_press_maybe(self.parsed_federation_invite_code_state_or.as_ref().map(
                        |parsed_federation_invite_code_state| {
                            app::Message::Routes(super::Message::BitcoinWalletPage(
                                Message::JoinFedimintFederation(
                                    parsed_federation_invite_code_state.invite_code.clone(),
                                ),
                            ))
                        },
                    )),
            );

        if let Some(parsed_federation_invite_code_state) =
            &self.parsed_federation_invite_code_state_or
        {
            container = container
                .push(Text::new("Federation ID").size(25))
                .push(Text::new(truncate_text(
                    &parsed_federation_invite_code_state
                        .invite_code
                        .federation_id()
                        .to_string(),
                    21,
                    true,
                )));

            match &parsed_federation_invite_code_state.loadable_federation_config {
                Loadable::Loading => {
                    container = container.push(Text::new("Loading..."));
                }
                Loadable::Loaded(client_config) => {
                    container = container
                        .push(Text::new("Federation Name").size(25))
                        .push(Text::new(
                            client_config
                                .meta::<String>(META_FEDERATION_NAME_KEY)
                                .ok()
                                .flatten()
                                .unwrap_or_default(),
                        ))
                        .push(Text::new("Modules").size(25))
                        .push(Text::new(
                            client_config
                                .modules
                                .values()
                                .map(|module| module.kind().to_string())
                                .collect::<Vec<_>>()
                                .join(", "),
                        ))
                        .push(Text::new("Guardians").size(25));
                    for peer_url in client_config.global.api_endpoints.values() {
                        container = container
                            .push(Text::new(format!("{} ({})", peer_url.name, peer_url.url)));
                    }
                }
                Loadable::Failed => {
                    container = container.push(Text::new("Failed to load client config"));
                }
            }
        }

        let nip_87_view: Element<app::Message> = match &self.nip_87_data_or {
            None => icon_button(
                "Find Federations",
                SvgIcon::Search,
                PaletteColor::Background,
            )
            .on_press(app::Message::Routes(super::Message::BitcoinWalletPage(
                Message::LoadNip87Federations,
            )))
            .into(),
            Some(Loadable::Loading) => Text::new("Loading...").into(),
            Some(Loadable::Loaded(federation_data)) => {
                let mut column = Column::new().spacing(10);

                let mut federation_data_sorted_by_recommendations: Vec<_> = federation_data
                    .iter()
                    .map(|(federation_id, (pubkeys, invite_codes))| {
                        (federation_id, pubkeys, invite_codes)
                    })
                    .collect();

                federation_data_sorted_by_recommendations
                    .sort_by_key(|(_, pubkeys, _)| pubkeys.len());
                federation_data_sorted_by_recommendations.reverse();

                // Filter out federations that we're already connected to.
                if let Loadable::Loaded(connected_federation_views) =
                    &connected_state.loadable_federation_views
                {
                    let connected_federation_ids =
                        connected_federation_views.keys().collect::<BTreeSet<_>>();

                    federation_data_sorted_by_recommendations.retain(|(federation_id, _, _)| {
                        !connected_federation_ids.contains(federation_id)
                    });
                }

                for (federation_id, pubkeys, invite_codes) in
                    federation_data_sorted_by_recommendations
                {
                    let mut sub_column = Column::new()
                        .push(Text::new(format!("Federation ID: {federation_id}")))
                        .push(Text::new(format!("{} recommendations", pubkeys.len())));

                    let mut loading_invite_codes: Vec<&ParsedFederationInviteCodeState> =
                        Vec::new();
                    let mut loaded_invite_codes: Vec<&ParsedFederationInviteCodeState> = Vec::new();
                    let mut errored_invite_codes: Vec<&ParsedFederationInviteCodeState> =
                        Vec::new();
                    for invite_code in invite_codes {
                        match &invite_code.loadable_federation_config {
                            Loadable::Loading => {
                                loading_invite_codes.push(invite_code);
                            }
                            Loadable::Loaded(_) => {
                                loaded_invite_codes.push(invite_code);
                            }
                            Loadable::Failed => {
                                errored_invite_codes.push(invite_code);
                            }
                        }
                    }

                    let mut most_progressed_invite_code_or = None;
                    // The order of priority is errored, loading, loaded.
                    // This is important because we don't want to consider a
                    // federation as errored if one of its invite codes is loading, and
                    // we don't want to consider a federation as loading if one of its
                    // invite codes has successfully loaded.
                    if !errored_invite_codes.is_empty() {
                        most_progressed_invite_code_or = Some(errored_invite_codes[0]);
                    } else if !loading_invite_codes.is_empty() {
                        most_progressed_invite_code_or = Some(loading_invite_codes[0]);
                    } else if !loaded_invite_codes.is_empty() {
                        most_progressed_invite_code_or = Some(loaded_invite_codes[0]);
                    }

                    if let Some(most_progressed_invite_code) = most_progressed_invite_code_or {
                        match &most_progressed_invite_code.loadable_federation_config {
                            Loadable::Loading => {
                                sub_column = sub_column.push(Text::new("Loading client config..."));
                            }
                            Loadable::Loaded(client_config) => {
                                sub_column = sub_column
                                    .push(Text::new("Federation Name").size(25))
                                    .push(Text::new(
                                        client_config
                                            .meta::<String>(META_FEDERATION_NAME_KEY)
                                            .ok()
                                            .flatten()
                                            .unwrap_or_default(),
                                    ))
                                    .push(Text::new("Modules").size(25))
                                    .push(Text::new(
                                        client_config
                                            .modules
                                            .values()
                                            .map(|module| module.kind().to_string())
                                            .collect::<Vec<_>>()
                                            .join(", "),
                                    ))
                                    .push(Text::new("Guardians").size(25));
                                for peer_url in client_config.global.api_endpoints.values() {
                                    sub_column = sub_column.push(Text::new(format!(
                                        "{} ({})",
                                        peer_url.name, peer_url.url
                                    )));
                                }

                                sub_column = sub_column.push(
                                    icon_button(
                                        "Join Federation",
                                        SvgIcon::Groups,
                                        PaletteColor::Primary,
                                    )
                                    .on_press(
                                        app::Message::Routes(super::Message::BitcoinWalletPage(
                                            Message::JoinFedimintFederation(
                                                most_progressed_invite_code.invite_code.clone(),
                                            ),
                                        )),
                                    ),
                                );
                            }
                            Loadable::Failed => {
                                sub_column =
                                    sub_column.push(Text::new("Failed to load client config"));
                            }
                        }
                    }

                    column = column.push(
                        Container::new(sub_column)
                            .padding(10)
                            .width(Length::Fill)
                            .style(|theme: &Theme| -> Style {
                                Style {
                                    text_color: None,
                                    background: Some(
                                        lighten(theme.palette().background, 0.05).into(),
                                    ),
                                    border: Border {
                                        color: iced::Color::WHITE,
                                        width: 0.0,
                                        radius: (8.0).into(),
                                    },
                                    shadow: Shadow::default(),
                                }
                            }),
                    );
                }

                column.into()
            }
            Some(Loadable::Failed) => Text::new("Failed to load NIP-87 data").into(),
        };

        container = container.push(nip_87_view);

        container = container.push(
            icon_button("Back", SvgIcon::ArrowBack, PaletteColor::Background).on_press(
                app::Message::Routes(super::Message::Navigate(RouteName::BitcoinWallet(
                    SubrouteName::List,
                ))),
            ),
        );

        container
    }
}
