from transformers import GPTNeoXForCausalLM, AutoTokenizer
from datetime import datetime
import csv

def generate():
    model = GPTNeoXForCausalLM.from_pretrained(
    "EleutherAI/pythia-160m-deduped",
    revision="step3000",
    cache_dir="./pythia-160m-deduped/step3000",
    )

    tokenizer = AutoTokenizer.from_pretrained(
    "EleutherAI/pythia-160m-deduped",
    revision="step3000",
    cache_dir="./pythia-160m-deduped/step3000",
    )
    inputs = tokenizer("how do i make money?", return_tensors="pt")
    tokens = model.generate(**inputs, max_new_tokens=256, do_sample=True, temperature=1, top_p=1, top_k=40)
    txt = tokenizer.decode(tokens[0])

    print(txt)

