<html>
    <head>
        <link rel="stylesheet" href="styles.css"/>
    </head>
    <body>
    <table>
    <tr>
        <th><i>Target</i></th>
        <th><i>Successor</i></th>
        <th><i>Frequency</i></th>
    </tr>
{

    let $xmlfiles := collection('/Users/zeynepkurtulus/Desktop/QMUL/Semi Structured/Coursework 1/cw1files?select=*.xml')//s  (: extracts all the sentences :)
    let $all_tuples := 
      for $s in $xmlfiles
        let $words  := $s/w  (: extraxts the words in each sentence :)
            for $w in $words
               where lower-case(normalize-space($w/text())) eq 'has' 
                 let $has_word := lower-case(normalize-space($w/text())) (: if the word is 'has' :)
                 let $successor := lower-case(normalize-space((data($w/following-sibling::w[1]))))
                 return
                    <tuple> {$has_word}:{$successor} </tuple>
     
     let $tuples := $all_tuples/text() (: gets rid of the tuples tag just the words: w1:w2 :)
     for $distinct_tuples in distinct-values($tuples) (: extract the unique tuples :)
        let $repeats := $tuples[. = $distinct_tuples] (: stores it if the distinct tuple matches with any other tuple in the whole list :)
        let $count := count($repeats)
        let $target := substring-before( $distinct_tuples, ':')
        let $successor := substring-after( $distinct_tuples , ':')
        order by $count descending
           return
                 <tr>
                     <td>{$target}</td>
                     <td>{$successor}</td>
                     <td>{$count}</td>
                 </tr>
                    
                    
}
    </table>
    </body>
</html>