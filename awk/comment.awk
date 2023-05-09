
BEGIN { getline r; }

/^[[:blank:]]*\*\/$/ { if (match(r, /^[[:blank:]]*\/\*/)) { sub(/\/\*/, "//", r); print r; getline r; next }}
{ print r;  r=$0; }

END { print r; }