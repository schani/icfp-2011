
	{	h=substr($0,2);
		H[h]++;
	}
	
END	{	for (n in H) {
			printf ("%5d\t%5d\t%s\n", H[n], length(n), n);
		}
	}
