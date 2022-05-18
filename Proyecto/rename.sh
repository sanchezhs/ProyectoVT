a=1
for i in *.json; do
	new=$(printf "%04d.json" "$a")
	mv -i -- "$i" "$new"
	let a=a+1
done
