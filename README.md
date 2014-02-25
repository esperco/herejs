herejs
======

Heredoc preprocessor for Javascript.

```
'''Hello
         ${name}!'''
```

becomes:

```javascript
'Hello\n         ' + name + '!'
```
