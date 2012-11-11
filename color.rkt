(module colors
 (import byte)
 (export shift-to-bytes Red Green Blue)
 (data Color ()
       (Red)
       (Green)
       (Blue)
       (Intensify Color))
 ;; color-to-bytes :: color -> [byte]
 (defn color-to-bytes (c)
   (case c
     ((Red) -> (byte "red"))
     ((Green) -> (byte "green"))
     ((Blue) -> (byte "blue"))
     ((Intensify x) -> (concat-bytes (byte "intensify") (color-to-bytes x)))))
 
 ;; shift :: color -> color
 (defn shift (c)
   (case c
     ((Red) -> Green)
     ((Green) -> Blue)
     ((Blue) -> Red)
     ((Intensify c) -> (Intensify (shift c)))))
 
 (defn shift-to-bytes (c)
   (color-to-bytes (shift c))))
