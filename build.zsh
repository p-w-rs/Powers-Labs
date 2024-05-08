#!/bin/zsh

statdir="./static"
srcdir="./src"
elmlist=()

if [[ $# -eq 0 ]]; then
    for file in "$srcdir"/*.elm; do
        if [[ -f $file ]]; then
            name=${file##*/}  # Extract filename without path
            name=${name%.*}    # Remove extension
            elmlist+=("$name")  # Add name to the list
        fi
    done
else
    elmlist=("$@")  # Use provided arguments as the list
fi

# Loop through the elm file list
for name in "${elmlist[@]}"; do
    js="$statdir/$name.js"
    min="$statdir/$name.min.js"

    echo "Processing $name..."

    elm make --optimize --output="$js" "$srcdir/$name.elm"

    if [[ $? -eq 0 ]]; then
        echo "Successfully compiled $name"
        uglifyjs "$js" --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output "$min"

        if [[ $? -eq 0 ]]; then
            echo "Successfully minified $name"
            echo "Compiled size:    $(stat -f%z "$js") bytes ($js)"
            echo "Minified size:    $(stat -f%z "$min") bytes ($min)"
            echo "Gzipped size: $(gzip -c "$min" | wc -c) bytes ($min.gz)"
        else
            echo "Error occurred while minifying $name"
        fi
    else
        echo "Error occurred while compiling $name"
    fi
    
    echo "Processing for $name completed."
    echo "------------------"
    echo  "\n\n"
done
