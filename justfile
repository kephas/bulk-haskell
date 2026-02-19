ghcid:
	ghcid --command "cabal --enable-multi-repl v2-repl bulk-test bulk" --restart bulk.cabal --reload config/ --reload test/bulk/ --warnings --test Main.main
