BEGIN { getline; sub(/;$/,""); r=$0 }
/^[[:blank:]]*\{[[:blank:]]*$/ { print r " {"; getline r; next }
{ print r;  r=$0; }
END { print r; }