def ubuntu_gte_18_10(plat_version: str) -> bool:
    try:
        platform_version_elts = plat_version.split('.')
        if len(platform_version_elts) < 2:
            return False
        major_version = int(platform_version_elts[0])
        minor_version = int(platform_version_elts[1])
        return major_version > 18 or major_version == 18 and minor_version >= 10
    except ValueError:
        return False

def ubuntu_lt_18_10(plat_version: str) -> bool:
    try:
        platform_version_elts = plat_version.split('.')
        if len(platform_version_elts) < 2:
            return False
        major_version = int(platform_version_elts[0])
        minor_version = int(platform_version_elts[1])
        return major_version < 18 or major_version == 18 and minor_version < 10
    except ValueError:
        return False
