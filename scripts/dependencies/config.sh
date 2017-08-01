# Name of the third party directory.
export THIRD_PARTY_DIR="third_party"

# Use a different dependency installer as fallback.
export FALLBACK_DIR="$INSTALLER_DIR/../../insieme/scripts/dependencies"

# Tries to load a package from INSTALLER_DIR or FALLBACK_DIR.
load_package() {
	if [[ -a "$INSTALLER_DIR/$1" ]]; then
		source "$INSTALLER_DIR/$1"
	elif [[ -n ${FALLBACK_DIR+x} && -a "$FALLBACK_DIR/$1" ]]; then
		source "$FALLBACK_DIR/$1"
	else
		>&2 echo "Error: no file $1 found"
		exit 1
	fi
}
export -f load_package

# Returns the property set inside a package.
get_property() {
	echo $(echo "load_package \"package_$1.sh\" && echo -n \$$2" | bash -)
}
export -f get_property

# Returns the prefix of an installed package.
get_pkg_prefix() {
	(
		load_package "package_$1.sh"
		if pkg_is_globally_installed ; then
			echo -n /usr
		else
			echo -n "$PREFIX/$PACKAGE"
		fi
	)
}
export -f get_pkg_prefix
