# Same capture name in alternation branches
"hello" =~ /(?<w>\d+)|(?<w>[a-z]+)/;
print "w=$+{w}\n";   # should be "hello"

"123" =~ /(?<w>\d+)|(?<w>[a-z]+)/;
print "w=$+{w}\n";   # should be "123"

# 5 named groups
"a1b2c3" =~ /(?<a>[a-z])(?<n1>\d)(?<b>[a-z])(?<n2>\d)(?<c>[a-z])(?<n3>\d)/;
print "$+{a}$+{n1}$+{b}$+{n2}$+{c}$+{n3}\n";  # should be "a1b2c3"
