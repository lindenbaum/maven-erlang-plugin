Nonterminals tag elements element start_tag end_tag .
Terminals 'atom' '<' '>' '/'.
Rootsymbol tag.
tag -> 
  start_tag tag end_tag : 
  ['$1', '$2', '$3'].
tag -> 
  start_tag tag tag end_tag : 
  ['$1', '$2', '$3', '$4'].
tag -> 
  start_tag elements end_tag : 
  ['$1', {'contents','$2'}, '$3'].   
tag -> 
  start_tag end_tag : 
  ['$1','$2'].
start_tag -> '<' 'atom' '>' : {'open','$2'}.   
end_tag -> '<' '/' 'atom' '>' : {'close','$3'}.   
elements -> element : ['$1'].
elements -> element elements : ['$1', '$2'].
element -> atom : '$1'.