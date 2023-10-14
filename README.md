# OCaml Problems
## Append new item of same type to the end of a list. (easy)

### Objective
Write a function that takes a list and a new item of the same type and appends the new item to the end of the list.

### Example
```ocaml
append_last [1; 2; 3] 4 = [1; 2; 3; 4]
append_last ["a"; "b"; "c"] "d" = ["a"; "b"; "c"; "d"]
```

### Solution
```ocaml
let append_last list item =
  list @ [item]
```


## Find the last but one (last and penultimate) elements of a list. (easy)

### Objective
Write a function that returns the last but one (last and penultimate) elements of a list.

### Example
```ocaml
last_two [1; 2; 3; 4] = (3, 4)
last_two ["a"; "b"; "c"; "d"] = ("c", "d")
```

### Solution
```ocaml
let rec last_two = function
  | [] | [_] -> None
  | [x; y] -> Some (x, y)
  | _ :: t -> last_two t
```

## Find the k'th element of a list. (easy)

### Objective
Write a function that returns the k'th element of a list. If the list is too short, return `None`.

### Example
```ocaml
at 3 [1; 2; 3; 4; 5] = Some 3
at 3 ["a"; "b"; "c"; "d"; "e"] = Some "c"
at 3 [1; 2] = None
```

### Solution
```ocaml
let rec at k = function
  | [] -> None
  | h :: t -> if k = 1 then Some h else at (k - 1) t
```

## Find the number of elements of a list. (easy)

### Objective
Write a function that returns the number of elements in a list.

### Example
```ocaml
length [1; 2; 3; 4; 5] = 5
length ["a"; "b"; "c"] = 3
length [] = 0
```

### Solution
```ocaml
let rec length = function
  | [] -> 0
  | _ :: t -> 1 + length t
```

## Reverse a list. (easy)

### Objective
Write a function that reverses a list.

### Example
```ocaml
reverse [1; 2; 3; 4; 5] = [5; 4; 3; 2; 1]
reverse ["a"; "b"; "c"] = ["c"; "b"; "a"]
```

### Solution
```ocaml
let rec reverse = function
  | [] -> []
  | h :: t -> reverse t @ [h]
```
