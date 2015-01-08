import os.path

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
                    self.example(line.split(":")[1].strip())
                else:
                    self._body += line

    def example(self, name):
        with open(os.path.join(os.path.dirname(__file__), "..", "examples", name)) as f:
            # Drop license text
            for n in range(15):
                f.readline()
            self._body += "```haskell\n"
            self._body += f.read()
            self._body += "```\n"
            self._body += "\n"
            self._body += "*Source code: (%s)[%s]. Generated C code: (%s)[%s].*\n" % (
                "examples/%s.hs" % name,
                "examples/%s.hs" % name,
                "examples/%s.c" % name,
                "examples/%s.c" % name,
            )

    def write(self):
        with open(self._path, "w") as f:
            f.write(self._toc + self._body)

def slug(title_line):
    return title_line.strip().lower().replace(" ", "-")

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
    ],
    "contributing.md",
    "license.md",
    "this-document.md",
])
