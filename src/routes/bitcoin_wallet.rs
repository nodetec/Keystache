use std::{collections::BTreeMap, str::FromStr};

use fedimint_core::{
    config::{ClientConfig, FederationId, META_FEDERATION_NAME_KEY},
    invite_code::InviteCode,
    Amount,
};
use iced::{
    widget::{
        column, container::Style, horizontal_space, row, text_input, Column, Container, Space, Text,
    },
    Border, Length, Shadow, Task, Theme,
};

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

    // LoadedNip87Federations(Vec<(InviteCode, ClientConfig)>),
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

                    Task::perform(
                        async move {
                            match fedimint_api_client::download_from_invite_code(&invite_code).await
                            {
                                Ok(config) => {
                                    app::Message::Routes(super::Message::BitcoinWalletPage(
                                        Message::LoadedFederationConfigFromInviteCode {
                                            config_invite_code: invite_code,
                                            config,
                                        },
                                    ))
                                }
                                // TODO: Include error in message and display it in the UI.
                                Err(_err) => {
                                    app::Message::Routes(super::Message::BitcoinWalletPage(
                                        Message::FailedToLoadFederationConfigFromInviteCode {
                                            config_invite_code: invite_code,
                                        },
                                    ))
                                }
                            }
                        },
                        |msg| msg,
                    )
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
                        *maybe_loading_federation_config = Loadable::Loaded(config);
                    }
                }

                Task::none()
            }
            Message::FailedToLoadFederationConfigFromInviteCode { config_invite_code } => {
                let Subroute::Add(Add {
                    parsed_federation_invite_code_state_or,
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

                Task::none()
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
}

pub struct ParsedFederationInviteCodeState {
    invite_code: InviteCode,
    loadable_federation_config: Loadable<ClientConfig>,
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
