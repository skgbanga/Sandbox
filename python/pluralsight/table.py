class Table:
    def __init__(self, header, *data):
        self.header = header
        self.data = data

    def __str__(self):
        m = 0
        for h in self.header:
            m = max(m, len("{}".format(h)))

        for col in self.data:
            for value in col:
                m = max(m, len("{}".format(value)))

        m = m + 2

        result = ""
        for h in self.header:
            result += "{:^{}}".format(h, m)
        result += "\n"

        for _ in self.header:
            result += "{:^{}}".format("=" * (m - 2), m)
        result += "\n"

        for row in zip(*self.data):
            for r in row:
                result += "{:^{}}".format(r, m)
            result += "\n"

        return result

    def _column_width(self, i):
        result = max(len(str(x)) for x in self.data[i])
        return max(len(self.header[i]), result)

    def __repr__(self):
        col_count = len(self.header)
        col_widths = [self._column_width(i) for i in range(col_count)]
        format_specs = ["{{:{}}}".format(col_widths[i]) for i in range(col_count)]

        result = []
        result.append(format_specs[i].format(self.header[i]) for i in range(col_count))
        result.append("=" * col_widths[i] for i in range(col_count))
        for row in zip(*self.data):
            result.append([format_specs[i].format(row[i]) for i in range(col_count)])

        result = (" ".join(r) for r in result)
        return "\n".join(result)
