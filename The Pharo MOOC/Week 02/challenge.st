| hash url |

hash := (MD5 hashMessage: 'minond.marcos@gmail.com') hex.

url := 'http://www.gravatar.com/avatar/',hash,'.jpg'.

(ZnEasy getJpeg: url) asMorph openInWindow 
