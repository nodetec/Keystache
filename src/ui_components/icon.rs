use iced::{
    widget::{
        svg::{self, Handle},
        Svg,
    },
    Color, Theme,
};

const CIRCLE_SVG_BYTES: &[u8] = "<svg xmlns=\"http://www.w3.org/2000/svg\" height=\"24px\" width=\"24px\" viewBox=\"0 0 100 100\"><circle cx=\"50\" cy=\"50\" r=\"40\" fill=\"black\" /></svg>".as_bytes();

#[derive(Clone, Copy)]
pub enum SvgIcon {
    Add,
    ArrowBack,
    ArrowDownward,
    ArrowUpward,
    Casino,
    ChevronRight,
    Circle,
    ContentCopy,
    CurrencyBitcoin,
    Delete,
    FileCopy,
    Groups,
    Home,
    Hub,
    Info,
    Key,
    Lock,
    LockOpen,
    Save,
    Search,
    Send,
    Settings,
    ThumbDown,
    ThumbUp,
}

macro_rules! icon_handle {
    ($icon:expr) => {
        Svg::new(Handle::from_memory(include_bytes!(concat!(
            "../../assets/icons/",
            $icon
        ))))
    };
}

impl SvgIcon {
    pub fn view<'a>(self, width: f32, height: f32, color: Color) -> Svg<'a, Theme> {
        match self {
            Self::Add => icon_handle!("add.svg"),
            Self::ArrowBack => icon_handle!("arrow_back.svg"),
            Self::ArrowDownward => icon_handle!("arrow_downward.svg"),
            Self::ArrowUpward => icon_handle!("arrow_upward.svg"),
            Self::Casino => icon_handle!("casino.svg"),
            Self::ChevronRight => icon_handle!("chevron_right.svg"),
            Self::Circle => Svg::new(Handle::from_memory(CIRCLE_SVG_BYTES)),
            Self::ContentCopy => icon_handle!("content_copy.svg"),
            Self::CurrencyBitcoin => icon_handle!("currency_bitcoin.svg"),
            Self::Delete => icon_handle!("delete.svg"),
            Self::FileCopy => icon_handle!("file_copy.svg"),
            Self::Groups => icon_handle!("groups.svg"),
            Self::Home => icon_handle!("home.svg"),
            Self::Hub => icon_handle!("hub.svg"),
            Self::Info => icon_handle!("info.svg"),
            Self::Key => icon_handle!("key.svg"),
            Self::Lock => icon_handle!("lock.svg"),
            Self::LockOpen => icon_handle!("lock_open.svg"),
            Self::Save => icon_handle!("save.svg"),
            Self::Search => icon_handle!("search.svg"),
            Self::Send => icon_handle!("send.svg"),
            Self::Settings => icon_handle!("settings.svg"),
            Self::ThumbDown => icon_handle!("thumb_down.svg"),
            Self::ThumbUp => icon_handle!("thumb_up.svg"),
        }
        .style(move |_, _| svg::Style { color: Some(color) })
        .width(width)
        .height(height)
    }
}
