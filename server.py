import http.server
PORT = 8020

class Handler(http.server.SimpleHTTPRequestHandler):
    def end_headers(self):
        self.send_header("Cache-Control", "no-cache, must-revalidate, max-age=0")
        http.server.SimpleHTTPRequestHandler.end_headers(self)

print("serving app at http://localhost:" + str(PORT) + "/")
http.server.HTTPServer(('', PORT), Handler).serve_forever()

