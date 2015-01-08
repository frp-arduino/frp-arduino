import os.path

class Document(object):

    def __init__(self, name):
        self._name = name
        self._path = os.path.join(os.path.dirname(__file__),
                                                  "..",
                                                  name)
        self._toc = ""
        self._body = ""

    def cat(self, name):
        with open(os.path.join(os.path.dirname(__file__), name)) as f:
            title_line = f.readline()
            self._toc += "* [%s](#%s)\n" % (title_line.strip(), slug(title_line))
            self._body += title_line
            for line in f:
                if line.startswith("INCLUDE_EXAMPLE:"):
                    self.example(line.split(":")[1].strip())
                else:
                    self._body += line

    def example(self, name):
        with open(os.path.join(os.path.dirname(__file__), "..", "examples", name)) as f:
            self._body += "```haskell\n"
            self._body += f.read()
            self._body += "```\n"

    def write(self):
        with open(self._path, "w") as f:
            f.write(self._toc + "\n" + self._body)

def slug(title_line):
    return title_line.strip().lower().replace(" ", "-")

def generate(path, docs):
    doc = Document(path)
    for x in docs:
        doc.cat(x)
    doc.write()

generate("README.md", ["intro.md"])
