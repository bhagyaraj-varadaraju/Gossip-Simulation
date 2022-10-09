rm ebin/*.beam

erlc -o ebin src/*.erl

erl -pa "ebin" -eval "supervising_boss:main($1, $2, $3)." -s init stop -noshell