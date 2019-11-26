echo "**recip	^=.*|^\[*[0-9][0-9]*\.*" > rend.txt
echo "**kern	^=.*|[A-Ga-g]{1,3}[-#]*[\]_]*|r" >> rend.txt
echo "**pc	^=.*|^[A-G][#b]*|r" > rend2.txt

for dir in 'inst/extdata/'
do
	for i in $dir*.krn
	do
		echo $i
		sed 's/[)(]*//g' $i | sed 's/ry/r/g' | sed 's/X//g'  | ridx -d > cur.krn
		ridx -G cur.krn | sed 's/[^;]//g' | awk '{print length }' > ferm.tmp
		extract -i '**kern' cur.krn |  semits -x | ditto -p > krn.tmp
		beat -u 1 -pd cur.krn  | sed 's/[LJ]*//g' | rend -i '**kern' -f rend.txt | sed 's/\[//g' | sed 's/[A-Ga-g]*[#-]*[]_]/./g' | pitch -x | rend -i '**pitch' -f rend2.txt | ditto -p -s = | num -a '**measure' -n '	=' | ditto > krnn.tmp

			awk 'BEGIN{FS="\t";cur=""}{
					 if ($1 ~ "^\\*M[1-9]") cur = $1
					   if ($0 ~ "^[*=!]") 
						{
						print $1 ;
					   }
					   else
						{
						  print substr(cur,3);
					   }}' cur.krn | sed 's/\*\*kern/**meter/g' | sed 's/\*I.*/*/g' > met.tmp



			paste krn.tmp krnn.tmp met.tmp | ridx -G > x.tmp
			paste x.tmp ferm.tmp > data/$(basename $i .krn).dat

			rm *.tmp
	done
done

rm rend*.txt
