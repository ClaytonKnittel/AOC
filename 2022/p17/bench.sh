
PROG=target/release/p17

set -e

for i in {1..100}
do
  python3 rand_wind.py 1000 > input.txt
  $PROG -n10000 -t
  # sleep 0.1
done | awk '{a += $1; b += 1} END{print a/b}'
