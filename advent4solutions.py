import re
f = open('advent/advent4input')
passports = f.read().split("\n\n")

def hgt_validator(hgt):
    height = int(hgt[:-2])
    unit = hgt[-2:]
    if unit == "cm":
        return height >= 150 and height <= 193
    else:
        return height >= 59 and height <= 76

validations = {
    "byr": lambda x: int(x) >= 1920 and int(x) <= 2002,
    "iyr": lambda x: int(x) >= 2010 and int(x) <= 2020,
    "eyr": lambda x: int(x) >= 2020 and int(x) <= 2030,
    "hgt": hgt_validator
}

valid = 0
for passport in passports:
    #Ignores immediately invalid values
    regex = r"(byr:[0-9]{4})|(iyr:[0-9]{4})|(eyr:[0-9]{4})|(hgt:[0-9]{,3}(cm|in))|(hcl:#[0-9a-f]{6})|(ecl:((amb)|(blu)|(brn)|(gry)|(grn)|(hzl)|(oth)))|(\bpid:[0-9]{9}\b)"
    matches = re.findall(regex, passport)
    #Reduce matches to first match
    matches = [tuple(j for j in i if j)[0] for i in matches]
    validated = True
    for match in matches:
        parts = match.split(":")
        key = parts[0]
        args = parts[1]
        #Check if validator exists
        if key in validations:
            #Call validator with args
            if not validations[key](args):
                validated = False
                break
    #If passport has all required fields and they are valid
    if len(matches) == 7 and validated:
        valid += 1 
print(valid)
f.close()
