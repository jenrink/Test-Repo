# Chat Bot Python Project - JEN RINK

import random
import readline
import sys



# First, we need the ChatBot to ask the User's Name.

print("BOT: what is your name?")
user_name = input()
print("BOT: What would you like to call me?")
bot_name = input()


# Now we create the templates. There are only two 'people' in this interaction: the User and the ChatBot.

bot_template = bot_name + " : {0}"
user_template = user_name + " : {0}"

# Now we create most of the conversation and responses. 
weather = "rainy"
mood = "happy"

responses = {
    "what's today's weather?": 
    ["The weather is {0}".format(weather), 
    "It's {0} today".format(weather), 
    "Let me check, it looks {0} today".format(weather) ],
    
    "are you a robot?": 
    ["What do you think?", 
    "Maybe yes, maybe no!", 
    "Yes, I am a robot with human feelings.", ],

    "how are you?": 
    ["I am feeling {0}".format(mood), 
    "{0}! How about you?".format(mood), 
    "I am {0}! How about yourself?".format(mood), ],

    "": 
    ["Hey! Are you there?", 
    "What do you mean by saying nothing?", 
    "Sometimes saying nothing tells a lot :)", ],

    "default": 
    ["this is a default message"]
}

def respond(message):
    if message in responses: 
        bot_message = random.choice(responses[message])
    else: 
        bot_message = random.choice(responses["default"])
    return bot_message

def related(x_text): 
    if "name" in x_text: 
        y_text = "what's your name?"
    elif "weather" in x_text: 
        y_text = "what's today's weather?"
    elif "robot" in x_text: 
        y_text = "are you a robot?"
    elif "how are" in x_text: 
        y_text = "how are you?"
    else: 
        y_text = ""
    return y_text

def send_message(message): 
  print(user_template.format(message)) 
  response = respond(message) 
  print(bot_template.format(response))


while 1:
    my_input = input() 
    my_input = my_input.lower() 
    related_text = related(my_input) 
    send_message(related_text)
    if my_input == "exit" or my_input == "stop": 
        break

