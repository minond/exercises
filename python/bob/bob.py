import re

def is_question(message):
    return message.endswith('?')

def is_yelling(message):
    return message.upper() == message and re.search('[a-zA-Z]+', message)

def is_nothing(message):
    return not message or message.strip() == ''

class Bob:
    def hey(self, message):
        if is_yelling(message):
            return 'Woah, chill out!'
        elif is_question(message):
            return 'Sure.'
        elif is_nothing(message):
            return 'Fine. Be that way!'
        else:
            return 'Whatever.'
