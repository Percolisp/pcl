# s530 running notes (Fable)

## 00:3x -- SESSION START; USER: "Please start. Do 3 subtasks at a time."
Box rebooted (uptime 2 min at 00:29); HEAVY.holder.1 held a dead pid (s527c's cut gate-set-scan) -- cleared.  CI GREEN both workflows on 26f57064.  Bare repo `pcl` = 6a5de5c1.  All three s529 checkpoints read (STOP.md x3): s526w2 332b34f8, s527b 8e4e6397, s527c cd7fe4b4 -- all over 2bf16931, records-only rebases.  Written: `s530/SHARED-BOX.md` + three resume briefs carrying the s529 rulings (push order s527b first; §37 apply with the fixture update; s526w2 base board dropped, det minus TT's 29 hangs; letters are the tool's) and ONE NEW RULING: **s527b's M3 rest (§9 §20 §14) MOVES to s527d** -- s527b goes straight to M4 + records + MERGE-READY (put the batch live first).

## 00:3x -- all three LAUNCHED (fresh opus, no isolation, in their worktrees) and verified: MODEL.txt = claude-opus-5-5 x3 (00:32:03 / :05 / :08).

## 01:4x -- USER: "two test suites can run in parallel" / "could there be 4?"  Measured: the 4G fence is a ceiling; legs peak ~1.8 G, 9 G free, but load 11/16 with two legs.  heavy.sh -> THREE slots (tested in an isolated lock dir: 3 concurrent, the 4th waits, an exclusive takes all three; installed by mv so the two running legs keep the old inode).  Four refused: CPU, and the timed runs lie under oversubscription.  SHARED-BOX amended.
