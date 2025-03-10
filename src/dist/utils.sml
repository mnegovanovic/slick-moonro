structure SMUtils = struct     
     val urlencode = String.translate (
        fn c => if Char.isAlphaNum c
                then String.str c
                else "%" ^ StringCvt.padLeft #"0" 2
                           (Int.fmt StringCvt.HEX (Char.ord c))
    )

    fun urldecode (v: string) =
        let
            val v = String.translate (fn #"+" => " " | c => String.str c) v
            fun process s = let
              val v = Word8.fromString (String.extract (s, 0, SOME 2))
            in
              String.concat [ String.str (Byte.byteToChar (valOf v)),
                              String.extract (s, 2, NONE) ]
            end handle
                Overflow => "%" ^ s
                | Subscript => "%" ^ s
                | Option => "%" ^ s
        in
            String.concat (case String.fields (fn c => c = #"%") v of
                nil => nil
                | x::rest => x::(map process rest))
        end

    fun strip to_strip chs =
        let
            fun stripLeft [] chs = []
                | stripLeft (x::xs) chs =
                    if (List.exists (fn c => c = x) chs) then
                        stripLeft xs chs
                    else
                        (x::xs)
            
            val stripped = stripLeft (String.explode to_strip) chs
            val stripped = stripLeft (rev stripped) chs
        in
            String.implode (rev stripped)
        end
    
    fun httpEncodePOSTArgs (args: (string*string)list): string =
        String.concatWith "&" (List.map (fn (k,v) => urlencode(k)^"="^urlencode(v)) args)

    fun findPairValue _ [] = NONE
        | findPairValue x ((k,v)::ks) = if k = x then SOME v else findPairValue x ks
end

