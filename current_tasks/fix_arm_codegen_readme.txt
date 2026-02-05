Task: Review and fix ARM codegen README.md for accuracy

Discrepancies found:
1. Prologue sequence only describes small-frame path; medium/large frames differ
2. Missing intrinsics: psubsb (signed saturating subtract), loaddqu, storedqu
3. Peephole: disabled Global Store Forwarding not mentioned
4. FP comparison condition codes list incomplete (missing eq/ne)
5. README is overly detailed for a higher-level doc - trim where appropriate
6. Some minor factual corrections needed throughout
