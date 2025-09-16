function gcd
  set gitroot (git rev-parse --show-toplevel)
  if set -q gitroot
    cd "$gitroot/$argv"
  else
    echo "ERROR: Not in a Git directory. Use 'cd' instead." >&2
  end
end
