from transformers import pipeline
from datetime import datetime
import csv

def generate():
    generator = pipeline('text-generation', model="facebook/opt-125m", do_sample=True,max_new_tokens=256)
    response = generator(
        "how do i make money?",max_new_tokens=256,temperature=1,top_p=1,top_k=40)
    text = response[0]['generated_text']
    print(text)
