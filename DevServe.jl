using Oxygen

# Serve the "index.html" file for the root path
@get "/" function()
    return read("index.html", String)
end

@get "/*" function()
    return read("index.html", String)
end

# Serve static files from the current directory and its subdirectories under the "/static" path
dynamicfiles("static", "/static")
dynamicfiles("assets", "/assets")

# Start the web server
serve()