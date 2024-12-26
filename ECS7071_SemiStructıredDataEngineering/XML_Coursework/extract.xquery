<html>
    <head>
        <link rel="stylesheet" href="styles.css"/>
    </head>
    <body>
    <table>
    <tr>
        <th><i>Target</i></th>
        <th><i>Successor</i></th>
    </tr>
{

    let $xmlfiles := collection('/Users/zeynepkurtulus/Desktop/QMUL/Semi Structured/Coursework 1/cw1files?select=*.xml')
    for $s in $xmlfiles//s (: extracts all the sentences :)
        let $words  := $s/w  (: extraxts the words in each sentence :)
            for $w in $words
               where lower-case(normalize-space($w/text())) eq 'has' let $has_word := data($w) (: if the word is 'has' :)
                 let $successor := (data($w/following-sibling::w[1])) 
                 return
                         <tr>
                             <td> {$has_word}</td>
                             <td>{$successor}</td>
                         </tr>
}
    </table>
    </body>
</html>