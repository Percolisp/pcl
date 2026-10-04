---
name: reference_task_store_json
description: Where task #NNN lives (~/.claude/tasks/pcl/NNN.json) and how to write one without corrupting its UTF-8
metadata:
  type: reference
---

Tasks referenced as `#NNN` in the PCL docs are JSON files at
`~/.claude/tasks/pcl/NNN.json` with keys `id, subject, description, status
(pending|in_progress|completed), blockedBy, blocks`.  Nothing in the repo
lists them; the docs cite numbers.

**Write them with `JSON::PP->new->utf8->canonical->pretty->encode` to a
`>:raw` handle** (read with `<:raw` + `->utf8->decode`).  Encoding a
character string with plain `->encode` and printing to a default handle
writes `§`/`½`/`×` as Latin-1 bytes and `—`/`→` as UTF-8 — a file that no
longer parses (s411 corrupted 379.json + the seven new ones that way;
repaired by re-decoding valid UTF-8 sequences and treating stray high bytes
as Latin-1).  Related: [[feedback_perl_i_slurp_truncates]].
