import os.path
import re

ROOT_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))

class Document(object):

    def __init__(self, name):
        self._name = name
        self._path = os.path.join(ROOT_DIR, name)
        self._toc = ""
        self._body = ""

    def cat(self, name, depth):
        with open(os.path.join(ROOT_DIR, "doc", name)) as f:
            self._body += "\n"
            for line in f:
                if line.startswith("INCLUDE_EXAMPLE:"):
                    self._read_example(line.split(":", 1)[1].strip())
                elif line.startswith("INCLUDE_VIDEO:"):
                    self._embed_youtube_video(line.split(":", 1)[1].strip())
                elif line.startswith("# "):
                    self._gen_toc(line[2:], depth)
                elif line.startswith("## "):
                    self._gen_toc(line[3:], depth+1)
                else:
                    self._body += self._replace_api_refs(line)

    def _gen_toc(self, title_line, depth):
        title = title_line.strip()
        self._toc += "%s* [%s](#%s)\n" % ("  "*depth, title, slug(title_line))
        self._body += "%s " % ("#" * (2+depth))
        self._body += title_line

    def _replace_api_refs(self, line):
        def replace(m):
            expression = m.group(1)
            parts = expression.split(".")
            name = parts[-1]
            html_file = "-".join(parts[:-1]) + ".html#" + sanitize(name)
            return "[`%s`](http://hackage.haskell.org/package/frp-arduino/docs/%s)" % (name, html_file)
        return re.sub(r"`api:(.+?)`", replace, line)

    def _read_example(self, name):
        with open(os.path.join(ROOT_DIR, "examples", name)) as f:
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

    def _embed_youtube_video(self, name):
        self._body += "".join([
            "<p align=\"center\">\n",
            "  <a href=\"http://youtu.be/%s\">\n" % name,
            "      <img src=\"http://img.youtube.com/vi/%s/0.jpg\">\n" % name,
            "  </a>\n",
            "</p>\n",
        ])

    def write(self):
        with open(self._path, "w") as f:
            f.write(self._toc + self._body)

def slug(title_line):
    return title_line.strip().lower().replace(" ", "-").replace(":", "")

def sanitize(name):
    return "v:" + name.replace("=", "-61-").replace("~", "-126-").replace(">", "-62-")

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
            "example-frequency-blink.md",
            "example-uart.md",
            "example-lcd.md",
        ],
        "api.md",
        "questions.md",
        "contributing.md",
        "resources.md",
        "license.md",
        "this-document.md",
    ])
