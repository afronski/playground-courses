var http = require("http"),
    fs = require("fs"),
    path = require("path");

http.createServer(function (request, response) {
    console.log("request starting... " + request.url);

    var filePath = "." + request.url,
        extname,
        contentType = "text/html";

    if (filePath === "./") {
        filePath = "./index.html";
    }

    extname = path.extname(filePath);

    switch (extname) {
        case ".js":
            contentType = "text/javascript";
            break;

        case ".ogg":
            contentType = "arraybuffer";
            break;
    }

    path.exists(filePath, function(exists) {

        if (exists) {
            fs.readFile(filePath, function(error, content) {
                if (error) {
                    response.writeHead(500);
                    response.end();
                } else {
                    response.writeHead(200, { "Content-Type": contentType });
                    response.end(content, "utf-8");
                }
            });
        } else {
            response.writeHead(404);
            response.end();
        }
    });

}).listen(8125);

console.log("Server running at http://127.0.0.1:8125/");