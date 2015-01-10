import os.path
import re

ROOT_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))

class Document(object):

    def __init__(self, name):
        self._name = name
        self._path = os.path.join(os.path.dirname(__file__),
                                                  "..",
                                                  name)
        self._toc = ""
        self._body = ""

    def cat(self, name, depth):
        with open(os.path.join(os.path.dirname(__file__), name)) as f:
            title_line = f.readline()
            title = title_line.strip()
            self._toc += "%s* [%s](#%s)\n" % ("  "*depth, title, slug(title_line))
            self._body += "\n%s " % ("#" * (2+depth))
            self._body += title_line
            for line in f:
                if line.startswith("INCLUDE_EXAMPLE:"):
                    self.example(line.split(":", 1)[1].strip())
                elif line.startswith("INCLUDE_VIDEO:"):
                    self.video(line.split(":", 1)[1].strip())
                elif line.startswith("INCLUDE_API:"):
                    self.api(line.split(":", 1)[1].strip())
                elif line.startswith("# "):
                    title_line = line[2:]
                    self._toc += "%s* [%s](#%s)\n" % ("  "*(depth+1), title, slug(title_line))
                    self._body += "\n%s " % ("#" * (3+depth))
                    self._body += title_line
                else:
                    self._body += self._replace_api_refs(line)

    def _replace_api_refs(self, line):
        def replace(m):
            name = m.group(1)
            return "[`%s`](#%s)" % (name, sanitize(name))
        return re.sub(r"`api:(.+?)`", replace, line)

    def example(self, name):
        with open(os.path.join(os.path.dirname(__file__), "..", "examples", name)) as f:
            # Drop license text
            for n in range(15):
                f.readline()
            self._body += "```haskell\n"
            self._body += f.read()
            self._body += "```\n"
            self._body += "\n"
            self._body += "* Source code: [%s](%s)\n" % (
                "examples/%s.hs" % name[:-3],
                "examples/%s.hs" % name[:-3],
            )
            self._body += "* Generated C code (no need to understand this): [%s](%s)\n" % (
                "examples/%s.c" % name[:-3],
                "examples/%s.c" % name[:-3],
            )
            self._body += "* Compile and upload command: `./make %s upload`\n" % name[:-3]

    def video(self, name):
        self._body += "<p align=\"center\">\n"
        self._body += "  <a href=\"http://youtu.be/%s\">\n" % name
        self._body += "      <img src=\"http://img.youtube.com/vi/%s/0.jpg\">\n" % name
        self._body += "  </a>\n"
        self._body += "</p>\n"

    def api(self, name):
        matches = 0
        for (root, dirs, files) in os.walk(os.path.join(ROOT_DIR, "src")):
            for file_ in files:
                with open(os.path.join(root, file_)) as f:
                    for line in f:
                        if line.startswith("%s :: " % name):
                            self._body += "<a name=\"%s\"></a>**%s**\n" % (sanitize(name), name)
                            self._body += "\n"
                            self._body += "```haskell\n"
                            self._body += line
                            self._body += "```\n"
                            matches += 1
        assert matches == 1, "api for %r" % name

    def write(self):
        with open(self._path, "w") as f:
            f.write(self._toc + self._body)

def slug(title_line):
    return title_line.strip().lower().replace(" ", "-").replace(":", "")

def sanitize(name):
    return "api-" + name.replace("=", "-61-").replace("~", "-126-").replace(">", "-62-")

def generate(path, files):
    def process(doc, files, depth):
        for x in files:
            if isinstance(x, list):
                process(doc, x, depth+1)
            else:
                doc.cat(x, depth)
    doc = Document(path)
    process(doc, files, 0)
    doc.write()

if __name__ == "__main__":
    generate("README.md", [
        "intro.md",
        "language.md", [
            "frp.md",
            "edsl.md",
            "compile-c.md",
        ],
        "examples.md", [
            "example-intro.md",
            "example-blink.md",
            "example-double-blink.md",
        ],
        "api.md",
        "contributing.md",
        "license.md",
        "this-document.md",
    ])
