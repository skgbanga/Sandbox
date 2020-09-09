from datetime import datetime


def get_earliest(*dates):
    def parts(date):
        mm, dd, yyyy = date.split("/")
        return yyyy, mm, dd

    return min(dates, key=parts)


def datetime_earliest(*dates):
    fmt = "%m/%d/%Y"
    m = min(datetime.strptime(d, fmt) for d in dates)
    return m.strftime(fmt)
