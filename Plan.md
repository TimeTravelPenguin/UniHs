# Application Planner

## Assessment Items Creator

Process:
1. Get current working directory. Find a parent folder that is a course (e.g. MATH2800)
2. In this course folder, if found, find (or create) an "Assessments" Folder.
3. In the Assessments folder, parse all folder names in the format "Assessment \d+". Create the next folder in the sequence. If none exist, or the largest is <= 0, create "Assessment 01".
4. In the new assessment folder, copy the template assessment folder into the new directory.

## Dev Notes
- Finalise config functions. Implement the effects of reading a config action: copying and text replacement
- Update `-Here` semantics (and other similar commands) to accept config template names. Also, look at `fileContentReplacementMappings`.
  Maybe use a data type to avoid the relative path in the key? Or maybe not?

## Issues
- Need to perform check before string replacing file, because it may not be a plaintext document.