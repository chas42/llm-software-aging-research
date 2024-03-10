from transformers import GPTNeoForCausalLM, GPT2Tokenizer
from datetime import datetime
import csv

def generate():
    model = GPTNeoForCausalLM.from_pretrained("EleutherAI/gpt-neo-125M")
    tokenizer = GPT2Tokenizer.from_pretrained("EleutherAI/gpt-neo-125M")

    inputs = tokenizer("how do i make money?", return_tensors="pt")
    tokens = model.generate(
        **inputs, 
        max_new_tokens=256, 
        do_sample=True, 
        temperature=1, top_p=1, 
        top_k=40
    )
    txt = tokenizer.decode(tokens[0])
    print(txt)