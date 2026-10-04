---
name: feedback_quote_shell_vars
description: "Never use a command-substitution shell var unquoted/unverified as a path argument — an empty var turned `grep -r PAT $DIR/` into a root-filesystem scan that ran for hours (s248b incident)"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: 08bda7ff-813c-4e18-95c0-fbe1dc80928b
---

# Quote and verify shell variables before using them as paths

**Incident (2026-06-13, s248b):** `MOODIR=$(perl -e '...')` failed under
strict → empty var → `grep -rn PAT $MOODIR.pm $MOODIR/` became
`grep -rn PAT .pm /` — a recursive scan of the ROOT filesystem that kept
running in the background for hours (user noticed the permission-denied
warnings on /swap.img, /boot/* and was alarmed it might be a security hole;
it was read-only as the user, but sloppy).

**How to apply:**
- Always run the command substitution and the consumer as SEPARATE Bash
  calls (dependency rule), and echo/verify the var is non-empty first.
- Quote every variable used as a path argument: `"$DIR"/...`.
- For locating module files, prefer `perl -MModule -e 'print $INC{"Module.pm"}'`
  in its own call, check the output, then use the literal path.
- If a background task produces unexpected warnings (scanning paths it has
  no business in), kill it immediately — don't leave it running.
