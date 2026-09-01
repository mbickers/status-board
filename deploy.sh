#!/usr/bin/env bash
set -euo pipefail

host="${STATUS_BOARD_HOST:-root@goose-art.maxbickers.com}"
cd "$(dirname "${BASH_SOURCE[0]}")"

# The schema files are pinned data, but intentionally not checked into Git.
mise run schemas

# Keep local build products and other ignored files out of the transfer.
git ls-files | rsync -a --files-from=- ./ "$host:/opt/status-board/"
rsync -a \
  server/mta_protobuf/gtfs-realtime.proto \
  server/mta_protobuf/gtfs-realtime-NYCT.proto \
  "$host:/opt/status-board/server/mta_protobuf/"

ssh "$host" 'set -e
cd /opt/status-board
if [ ! -e _opam/.opam-switch/switch-config ]; then
  OPAMJOBS=1 opam switch create . 5.1.1 --no-install --yes --repositories=default=https://opam.ocaml.org
fi
OPAMJOBS=1 opam install . --deps-only --locked --yes
OPAMJOBS=1 opam exec -- dune build server/bin/main.exe
install -d /var/lib/status-board/cache
install -m 644 status-board.service /etc/systemd/system/status-board.service
systemctl daemon-reload
systemctl enable status-board
systemctl restart status-board'
