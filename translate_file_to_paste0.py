import os
import sys

if len(sys.argv) != 2:
    print('\nPlease specify file to translate.\n\nUsage: {} /path/to/file\n'.format(sys.argv[0]))
    sys.exit(1)

file_to_translate = sys.argv[1]
if not os.path.isfile(file_to_translate):
    print('\n{} is not a valid file.\n'.format(file_to_translate))

with open(file_to_translate, 'r') as f:
    lines = f.readlines()

new_lines = ['var.name <- paste0(']

for line in lines:

    if r'{{{ignore}}}' in line.lower():
        continue

    line = line.replace('<', r'&lt;')
    line = line.replace('>', r'&gt;')
    line = line.replace('"', r'\"')

    line = r'  "{}\n",'.format(line.replace('\n',''))
    
    line = line.replace(r'~~~', r'", eval(parse(text=input$VAR_NAME)), "')

    new_lines.append(line)

new_lines.append('  ""\n)')


print('\n'.join(new_lines))