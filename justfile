ghcid:
	ghcid --command "cabal --enable-multi-repl v2-repl bulk-test bulk" --restart bulk.cabal --warnings --test Main.main
