# This script can be run on any supported OS to create a binary .tar.gz bundle.
# For Windows, msysgit contains all of the pieces needed to run this script.

set -e

feedback() {
    # ANSI codes need to be enclosed in `\[` and `\]` when
    # used with special bash variables PS0/1/2/4.
    # See https://stackoverflow.com/a/28938235 for a dope explanation
    echo -e "\\e[1;36m$1\\e[0m"
}

# Check usage
OS=$1
if [ -z "$OS" ]; then
    echo "Usage: build.sh osname"
    exit 1
fi

# Move to the project root
pushd "$(stack path --project-root)" >/dev/null

feedback "Creating staging directory"
mkdir -vp bundle/build/elm-smuggle

feedback "Copying built binary to staging directory"
BIN=elm-smuggle
BIN_EXT=""
if [ "$OS" = "win64" ]; then
    BIN_EXT=".exe"
fi
LOCAL_INSTALL_ROOT=$(stack path --local-install-root)
FULL_BIN="${LOCAL_INSTALL_ROOT}/bin/${BIN}${BIN_EXT}"
if [ "$OS" != "win64" ]; then
    strip "$FULL_BIN"
fi
cp -v "$FULL_BIN" bundle/build/elm-smuggle

feedback "Copying extra files to staging directory"
cp -v LICENSE bundle/build/elm-smuggle
echo "stack ls dependencies >bundle/build/elm-smuggle/dependencies.txt"
stack ls dependencies >bundle/build/elm-smuggle/dependencies.txt

feedback "Creating $OS.tar.gz"
pushd bundle/build >/dev/null
tar -zcvf "../$OS.tar.gz" elm-smuggle
popd >/dev/null

feedback "Calculating SHA hash for $OS.tar.gz"
if [ "$OS" = "win64" ]; then
    # msys/mingw doesn't have shasum :(
    SHASUM="openssl dgst -sha1"
else
    SHASUM="shasum"
fi
echo "$SHASUM bundle/$OS.tar.gz >bundle/$OS.sha"
$SHASUM "bundle/$OS.tar.gz" >"bundle/$OS.sha"

feedback "Cleaning up"
rm -vrf bundle/build

popd >/dev/null
