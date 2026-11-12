# Design notes

Notes written while working on a change: the problem, the options, what was
decided and why.  Each says at the top whether (and where) it was
implemented.  Newest first.

| Note | Date | Status |
| --- | --- | --- |
| [A tagless GC (layouts instead of tags)](2026-11-tagless-gc.md) | Nov 2026 | proposal |
| [The value restriction](2026-11-value-restriction.md) | Nov 2026 | done (#39) |
| [Source locations in s-expressions](2026-11-source-locations.md) | Nov 2026 | done (`srcloc`) |
| [Uncommitted work: survey and plan](2026-10-uncommitted-work.md) | Oct 2026 | done; historical |

Smaller decisions live in their commit messages:

| Decision | Commit |
| --- | --- |
| Integer division: SRFI 141 names; `/`, `mod`, `divmod` stay Euclidean | `196eb65` |
| Integer literals: rejected when too big; constants up to +/-2^62 | `91b361a`, `8d246cd` |
| Top-level definitions run in dependency order (#86) | `8c7293f` |
| A growing heap | `d355165` |
| No to-space clearing: initialize what a collection can see | `7110a22` |
