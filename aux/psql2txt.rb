#!/usr/bin/ruby

`rm -f timebank.sql`
`pg_dump -U research -h localhost data --no-owner --clean --file=tmp --table=timebank_doc`
`cat tmp >> timebank.sql`
`pg_dump -U research -h localhost data --no-owner --clean --file=tmp --table=timebank_sent`
`cat tmp >> timebank.sql`
`pg_dump -U research -h localhost data --no-owner --clean --file=tmp --table=timebank_tag`
`cat tmp >> timebank.sql`
`pg_dump -U research -h localhost data --no-owner --clean --file=tmp --table=timebank_timex`
`cat tmp >> timebank.sql`
`pg_dump -U research -h localhost data --no-owner --clean --file=tmp --table=timebank_tlink`
`cat tmp >> timebank.sql`
`rm tmp`
