Integrated tool for learning Finnish with Prolog. It's a WordNet database and a SuperMemo-2 implementation in one file. All data entry is done manually by inserting more lines into the file.

## Dependencies

- https://github.com/fnogatz/date_time

## Spaced repetition work flow
- Run a SWI-Prolog shell
- `[main].`
- `next(Entry).` and press `;` for more entries to study
- Manually rate the score and add memo entries to the file
- At the end of the session, reevaluate the file again to update the memo entries (`[main].`) then call `below_four(Entry)` to study entries that scored below 4. 
  Continue the repetitions until all of these items score at least four.
