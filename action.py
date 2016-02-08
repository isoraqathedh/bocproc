import exiftoolwithset
import sexpr

##### To be refactored in the future.

# Setup
config_file = configparser.RawConfigParser()
config_file_location = os.path.expanduser("~/Documents/AutoHotkey/scan.ini")
config_file.optionxform = lambda option: option
config_file.read(config_file_location, encoding='utf-16')
ets = ExiftoolWithSet()

# # Utility functions
# def fileexistp(glob_sepc):
#     """Does a file or folder exist?"""
#     if glob.glob(glob_spec): return True
#     else: return False

# def defaultp(thing):
#     return thing in {'default', 'auto', '', None}
