# Device firmware and protocol

Use the existing TRMNL firmware and protocol instead of implementing custom firmware or a new device protocol. TRMNL already handles displaying errors and has all the other intermediate functionality needed.

# Display refresh and colors

The E1001's full refresh takes about two to five seconds and visibly flashes the whole screen. Partial refresh changes only affected pixels and avoids most flashing. It accumulates ghosting, so periodic full refreshes remain necessary. We use partial refresh because routine updates are less distracting.

Four-gray output requires a full refresh in TRMNL firmware and Seeed's current driver. This is not necessarily a physical limitation of the panel, but experimenting would require custom firmware.

# Backend server

Use one server for fetching data and rendering displays. Each data source stores its latest result in a disposable disk cache with its own expiration time, so that we can restart server during development without worrying about spamming external API endpoints.

I considered adding a dev subcommand to the server that just outputs the image to filesystem, but it is fun to watch automatic reload in action and directly see debug information.

We are using OCaml for the backend because I wanted a functional language that I already know and it has mature protobuf support, which the MTA GTFS-Realtime feeds require.
