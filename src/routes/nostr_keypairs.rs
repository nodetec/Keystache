use std::str::FromStr;

use iced::{
    widget::{row, text_input, Column, Text},
    Task,
};
use nostr_sdk::{
    secp256k1::{rand::thread_rng, Keypair},
    SecretKey,
};
use secp256k1::Secp256k1;

use crate::{
    app,
    ui_components::{icon_button, PaletteColor, SvgIcon, Toast, ToastStatus},
    util::truncate_text,
};

use super::{container, ConnectedState, RouteName};

#[derive(Debug, Clone)]
pub enum Message {
    SaveKeypair(Keypair),
    SaveKeypairNsecInputChanged(String),
    DeleteKeypair { public_key: String },
}

pub struct Page {
    pub connected_state: ConnectedState,
    pub subroute: Subroute,
}

impl Page {
    pub fn update(&mut self, msg: Message) -> Task<app::Message> {
        match msg {
            Message::SaveKeypair(keypair) => match self.connected_state.db.save_keypair(&keypair) {
                Ok(()) => Task::done(app::Message::AddToast(Toast {
                    title: "Saved keypair".to_string(),
                    body: "The keypair was successfully saved.".to_string(),
                    status: ToastStatus::Good,
                })),
                Err(_err) => Task::done(app::Message::AddToast(Toast {
                    title: "Failed to save keypair".to_string(),
                    body: "The keypair was not saved.".to_string(),
                    status: ToastStatus::Bad,
                })),
            },
            Message::SaveKeypairNsecInputChanged(new_nsec) => {
                if let Subroute::Add(Add {
                    nsec, keypair_or, ..
                }) = &mut self.subroute
                {
                    *nsec = new_nsec;

                    // Set `keypair_or` to `Some` if `nsec` is a valid secret key, `None` otherwise.
                    *keypair_or = SecretKey::from_str(nsec).map_or(None, |secret_key| {
                        Some(Keypair::from_secret_key(&Secp256k1::new(), &secret_key))
                    });
                }

                Task::none()
            }
            Message::DeleteKeypair { public_key } => {
                match self.connected_state.db.remove_keypair(&public_key) {
                    Ok(()) => Task::done(app::Message::AddToast(Toast {
                        title: "Deleted keypair".to_string(),
                        body: "The keypair was successfully deleted.".to_string(),
                        status: ToastStatus::Good,
                    })),
                    Err(_err) => Task::done(app::Message::AddToast(Toast {
                        title: "Failed to delete keypair".to_string(),
                        body: "The keypair was not deleted.".to_string(),
                        status: ToastStatus::Bad,
                    })),
                }
            }
        }
    }

    pub fn view<'a>(&self) -> Column<'a, app::Message> {
        match &self.subroute {
            Subroute::List(list) => list.view(&self.connected_state),
            Subroute::Add(add) => add.view(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SubrouteName {
    List,
    Add,
}

impl SubrouteName {
    pub fn to_default_subroute(&self) -> Subroute {
        match self {
            Self::List => Subroute::List(List {}),
            Self::Add => Subroute::Add(Add {
                nsec: String::new(),
                keypair_or: None,
            }),
        }
    }
}

pub enum Subroute {
    List(List),
    Add(Add),
}

impl Subroute {
    pub fn to_name(&self) -> SubrouteName {
        match self {
            Self::List(_) => SubrouteName::List,
            Self::Add(_) => SubrouteName::Add,
        }
    }
}

pub struct List {}

impl List {
    // TODO: Remove this clippy allow.
    #[allow(clippy::unused_self)]
    fn view<'a>(&self, connected_state: &ConnectedState) -> Column<'a, app::Message> {
        // TODO: Add pagination.
        let Ok(public_keys) = connected_state.db.list_public_keys(999, 0) else {
            return container("Keys").push("Failed to load keys");
        };

        let mut container = container("Keys");

        for public_key in public_keys {
            container = container.push(row![
                Text::new(truncate_text(&public_key, 12, true))
                    .size(20)
                    .align_x(iced::alignment::Horizontal::Center),
                icon_button("Delete", SvgIcon::Delete, PaletteColor::Danger).on_press(
                    app::Message::Routes(super::Message::NostrKeypairsPage(
                        Message::DeleteKeypair { public_key }
                    ))
                ),
            ]);
        }

        container = container.push(
            icon_button("Add Keypair", SvgIcon::Add, PaletteColor::Primary).on_press(
                app::Message::Routes(super::Message::Navigate(RouteName::NostrKeypairs(
                    SubrouteName::Add,
                ))),
            ),
        );

        container
    }
}

pub struct Add {
    nsec: String,
    keypair_or: Option<Keypair>, // Parsed from nsec on any update. `Some` if nsec is valid, `None` otherwise.
}

impl Add {
    fn view<'a>(&self) -> Column<'a, app::Message> {
        container("Add Keypair")
            .push(
                text_input("nSec", &self.nsec)
                    .on_input(|input| {
                        app::Message::Routes(super::Message::NostrKeypairsPage(
                            Message::SaveKeypairNsecInputChanged(input),
                        ))
                    })
                    .padding(10)
                    .size(30),
            )
            .push(
                icon_button("Save", SvgIcon::Save, PaletteColor::Primary).on_press_maybe(
                    self.keypair_or.map(|keypair| {
                        app::Message::Routes(super::Message::NostrKeypairsPage(
                            Message::SaveKeypair(keypair),
                        ))
                    }),
                ),
            )
            .push(
                icon_button(
                    "Generate New Keypair",
                    SvgIcon::Casino,
                    PaletteColor::Primary,
                )
                .on_press(app::Message::Routes(super::Message::NostrKeypairsPage(
                    Message::SaveKeypair(Keypair::new_global(&mut thread_rng())),
                ))),
            )
            .push(
                icon_button("Back", SvgIcon::ArrowBack, PaletteColor::Background).on_press(
                    app::Message::Routes(super::Message::Navigate(RouteName::NostrKeypairs(
                        SubrouteName::List,
                    ))),
                ),
            )
    }
}
