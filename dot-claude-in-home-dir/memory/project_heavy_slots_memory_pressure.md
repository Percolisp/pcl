---
name: project_heavy_slots_memory_pressure
description: On this 12 GB laptop four heavy slots beside a browser got GNOME killed by systemd-oomd (s536); default to three unless the USER says the box is otherwise idle
metadata:
  type: project
---

2026-11-08 21:23: `systemd-oomd` killed GNOME Shell for memory pressure and took the terminal, the reviewing session and every agent and heavy leg with it (no reboot, nothing committed lost).  Four legs had started in one second when an exclusive bench released its slots (load 55), beside a browser that peaked at 7.9 GB.  Four slots had run clean for eight hours before the browser started.

**Why:** the USER's word for four slots was that the laptop was otherwise idle; a browser later made that false and nothing re-checked it.  `journalctl -b | grep -i oomd` names the killer in one line.

**How to apply:** `~/pcl-agent-scratch/s513/heavy.sh` is the three-slot script again (`heavy.sh.four-slot-s536` beside it).  Use four only on the USER's word that nothing else is running, for that sitting.  After such a kill: read each leg's `=== heavy.sh ... END` line (no END line = VOID), clear the dead `HEAVY.holder*`, resume from each worktree's `STOP.md`; the resume brief of s536 is `briefs-and-rules-for-claude-subagents/s536/resume-after-oom.md`.  Related: [[feedback_no_redundant_bg_wait]].
