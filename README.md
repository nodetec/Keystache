# Keystache

Keystache is a desktop Nostr key management and Bitcoin wallet.

## Description

Keystache is designed to provide a secure and user-friendly interface for managing Nostr keys and interacting with Fedimint Bitcoin federations. It offers features such as:

- Nostr key management (creation, storage, and deletion)
- Nostr relay management
- Bitcoin wallet functionality through Fedimint federations
- Secure, encrypted local storage of keys and data

## Features

- **Nostr Key Management**: Create, store, and manage Nostr keys securely.
- **Relay Management**: Connect to and manage Nostr relays.
- **Fedimint Bitcoin Wallet**: Send and receive Bitcoin through Fedimint federations.
- **Encrypted Storage**: All sensitive data is encrypted at rest.
- **User-Friendly Interface**: Built with Iced, a cross-platform GUI library for Rust.

## Installation

To build Keystache from source:

1. Ensure you have Rust and Cargo installed on your system.
2. Clone the repository:
   ```
   git clone https://github.com/Open-Source-Justice-Foundation/Keystache.git
   ```
3. Navigate to the project directory:
   ```
   cd Keystache
   ```
4. Build the project:
   ```
   cargo build --release
   ```
5. Run the application:
   ```
   cargo run --release
   ```

## Usage

After launching Keystache, you'll be prompted to create a new password or enter an existing one to unlock your database. Once inside, you can:

- Manage Nostr keys
- Connect to and manage Nostr relays
- Send and receive Bitcoin through Fedimint federations
- Adjust settings and view application information

## Development

Keystache is built using Rust and the Iced GUI library. The project structure is as follows:

- `src/`: Contains the main application code
- `src/db/`: Database-related code
- `src/fedimint/`: Fedimint integration
- `src/nostr/`: Nostr-related functionality
- `src/routes/`: Application routes and views
- `src/ui_components/`: Reusable UI components
- `assets/`: Contains icons and other static assets

## Contributing

Contributions to Keystache are welcome! Please feel free to submit pull requests, create issues, or suggest improvements.

## License

[Insert appropriate license information here]

## Acknowledgments

Keystache is created by Tommy Volk and generously funded by OpenSats.

## Contact

For questions or support, please [insert contact information or link to support resources].
