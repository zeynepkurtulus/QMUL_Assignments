<html>
    <head>
        <link rel="stylesheet" href="styles.css"/>
    </head>
    <body>
    <table>
    <tr>
        <th><i>Target</i></th>
        <th><i>Successor</i></th>
        <th><i>Probability</i></th>
    </tr>
    {
        let $xmlfiles := collection('/Users/zeynepkurtulus/Desktop/QMUL/Semi Structured/Coursework 1/cw1files?select=*.xml')//s
        let $all_tuples := 
            for $s in $xmlfiles
            let $words := $s/w
            for $w in $words
            where lower-case(normalize-space($w/text())) eq 'has'
            let $successor := (data($s/w[. >> $w][1]))
            return
                 <tuple> {lower-case(normalize-space($w/text()))}:{lower-case(normalize-space($successor))}</tuple>
                
         let $all := 
             for $s in $xmlfiles
             return 
             let $word := $s/w
             for $w in $word 
             return <word>{lower-case(normalize-space($w/text()))}</word>
             
             
        let $top20 := 
        for $distinct in distinct-values($all_tuples/text())
           let $count := count($all_tuples[text() = $distinct])
           let $target := substring-before($distinct, ':')
           let $successor := substring-after($distinct, ':')
           let $total_successors := count($all[. = $successor])
       
          let $prob := round-half-to-even($count div $total_successors, 2)
           order by $prob descending
           
           return
                 <tr>
                     <td>{$target}</td>
                     <td>{$successor}</td>
                     <td>{$prob}</td>
                 </tr>
                 
          return subsequence($top20,1,20)
    }
    </table>
    </body>
</html>
