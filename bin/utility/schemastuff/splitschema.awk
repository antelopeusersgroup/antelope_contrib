
BEGIN {
    PRINT = 0
    TYPE="Attribute"
    if (type=="r") TYPE="Relation"    
    VAR=var    
}

/^[ \t]*;/ {
    if (PRINT==1) print ;
    PRINT=0
}
{
    if ($1 == TYPE && $2 == VAR) PRINT=1
    if (PRINT==1) print $0
}
