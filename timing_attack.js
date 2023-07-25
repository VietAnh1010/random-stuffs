const { performance } = require("perf_hooks");
let pwd = "1112"

console.log("password length is: " + pwd.length);
// pwd = [...pwd];

function login(pw) {
	// return pwd == pw;
	// return typeof pw === "string" && pwd.length >= pw.length && pwd.every((c, i) => c === pw[i]);
	return !(Array.isArray(pw) || pw.length > pwd.length) && [...pwd].every((c, i) => c === pw[i]);
}

//const ALPHABET = "abcdefghijklmnABCDEFGHIJKLMN0123456789";
const ALPHABET = "0123456789"

function try_length(login) {
	let upper = 40;
	let s = "0".repeat(upper);
	let ss1 = 0;
	let ss2 = 0;
	let s1 = 0;
	let s2 = 0;
	let count = 0;
	let count_ = 0;
	let found = false;

	let length = -1;
	const timings = [];
	let prev = undefined;
	let avg = undefined;
	let std = 999999;

	let ratio = 0;

	for (let i = upper; i > 0; i--) {
		const arr = [];
		for (let k = 0; k < 10; k++) {
			const start = performance.now();
			for (let j = 0; j < 10; j++) {
				login(s);
			}
			const end = performance.now();
			arr.push(end - start);
		}
		// const min = arr.reduce((x, y) => x + y);
		const min = Math.min(...arr);
		// compare arr?
		// console.log(`[try_length iteration ${i} avg]: ${avg}`);
		// console.log(`[try_length iteration ${i} std]: ${std}`);
		const dif = min - prev;
		const rat = min / prev;
		if (dif > std * 3 && rat > ratio) {
			/*
			console.log("should update");
			if (!found) {
				found = true;
			} else {
				console.log("amibigious");
			}
			*/
			length = s.length;
			ratio = rat;
			/*
			s1 = 0;
			s2 = 0;
			*/
			count_ = 0;
			bestStd = std;

		}
		s1 += min;
		s2 += min ** 2;
		count++;
		count_++;
		avg = s1 / count;
		std = Math.sqrt(s2 / count - avg ** 2);

		// if (s.length < 40 && min - prev >= 0.0005 && !found) {
		//	found = true;
		//	length = s.length;
		// }
		prev = min;
		timings.push(min);
		s = s.substring(1);
	}
	/*
	timings.push(0);
	// we now have an array of data
	// we need to split this array
	timings.reverse();
	ss1 = timings.reduce((x, y) => x + y);
	ss2 = timings.map(x => x ** 2).reduce((x, y) => x + y);
	let bestStd = 999999;

	s1 = timings.slice(0, 5).reduce((x, y) => x + y);
	s2 = timings.slice(0, 5).map(x => x ** 2).reduce((x, y) => x + y)

	console.log(timings);
	const sumStds = [];
	for (let l = 5; l < 40; l++) {
		s1 += timings[l];
		s2 += timings[l] ** 2;
		let sumStd = 0;
		// calculate the avg of 2 sides
		avg = s1 / l;
		std = Math.sqrt(s2 / l - avg ** 2) * l;
		sumStd += std;

		avg = (ss1 - s1) / (50 - l);
		std = Math.sqrt((ss2 - s2) / (50 - l) - avg ** 2) * (50 - l);
		sumStd += std;

		if (sumStd < bestStd) {
			length = l;
			bestStd = sumStd;
		}
		sumStds.push(sumStd);
	}
	*/
		// console.log(sumStds);
		//
		if (length !== -1) {
			console.log(timings);
			console.log(length);
			return [length, timings];
		} else {
			console.log("some thing went wrong in this iteration");
			return [-1, []];
		}
}

function crack(login) {
	let pw = "";
	// const pwlength = try_length(login);
	// console.log(pwlength);

	function try_letter(c) {
		const guess = pw + c + "?!";
		const arr = [];
		for (let k = 0; k < 10; k++) {
			const start = performance.now();
			for (let j = 0; j < 1000; j++) {
				login(guess);
			}
			const end = performance.now();
			arr.push(end - start);
		}
		// const min = arr.reduce((x, y) => x + y);
		const min = Math.min(...arr);
		/*
		for (let k = 0; k < 1000; k++) {
			const start = performance.now();
			eval("login(guess);");
			const end = performance.now();
			arr[k] = end - start;
		}
		arr.sort();
		return arr.slice(450, 550).reduce((x, y) => x + y);
		*/
		return min;
	}

	for (let i = 0; i < pwd.length; i++) {
		const timings = [];
		for (const c of ALPHABET) {
			const guess = pw + c + "?!";
			const arr = [];
			for (let k = 0; k < 10; k++) {
				const start = performance.now();
				for (let j = 0; j < 1000; j++) {
					login(guess);
				}
				const end = performance.now();
				arr.push(end - start);
			}
			const min = Math.min(...arr);
			timings.push(min);
		}
		console.log([...ALPHABET].map((c, i) => [c, timings[i]]));
		/*
		let s1 = 0;
		let s2 = 0;
		for (const t of timings) {
			s1 += t;
			s2 += t ** 2;
		}
		const avg = s1 / timings.length;
		const std = Math.sqrt(s2 / timings.length - avg ** 2);

		console.log("avg: " + avg);
		console.log("std: " + std);

		let c;
		let found = false;
		for (let i = 0; i < timings.length; i++) {
			c = ALPHABET[i];
			const t = timings[i];
			if ((t - avg) > std) {
				pw += c;
				found = true;
			} else if (found) {
				throw Error("Ambigious");
			}
		}
		*/
		let mi = 0;
		let mt = 0;
		for (let i = 0; i < timings.length; i++) {
			if (timings[i] > mt) {
				mt = timings[i];
				mi = i;
			}
		}
		const c = ALPHABET[mi];
		console.log("find character: " + c);
		pw += c;
	}
	console.log(pw);
	return pw;
}

const crypto = require("crypto");

function randomString(length) {
	const limit = ALPHABET.length - 1;
	return Array.from({ length }, _ => ALPHABET[Math.round(Math.random() * limit)]).join('');	
	// return crypto.randomBytes(length).toString(hex);
}


function testTryLength() {
	const msgs = [];
	for (let i = 3; i <= 32; i++) {
		pwd = randomString(i);
		console.log(pwd);
		const [length, timings] = try_length(login);
		if (pwd.length !== length) {
			msgs.push(`[WARN] wrong length, expected ${pwd.length}, actual ${length}
			[DEBUG] ${timings}`);
		}
	}
	console.log(msgs);
}

// testTryLength();
pwd = randomString(32);

console.log(`pwd: ${pwd}`)
function crack_(login) {
	var guess = randomString(32);
	var index=0;
	var guess_t = 0;
	for(let iter=0; iter<10000; iter++) {
		for(var i=0; i<10; i++) {
			var alt=guess.substring(0, index) + i + guess.substring(index+1);
			if (login(alt))
				return alt;

			let arr = [];
			/*
			for (let k = 0; k < 10; k++) {
				const start = performance.now();
				for (let j = 0; j < 10; j++) {
					login(guess);
				}
				const end = performance.now();
				arr.push(end - start);
			}
			const minguess = Math.min(...arr);
			arr = [];
			*/
			for (let k = 0; k < 10; k++) {
				const start = performance.now();
				for (let j = 0; j < 10; j++) {
					login(alt);
				}
				const end = performance.now();
				arr.push(end - start);
			}
			const minalt = Math.min(...arr);
			if (minalt > guess_t) {
				guess = alt;
				guess_t = minalt;
			}
		}
		index = (index + 1) % 32;
	}
	console.log("ans: " + guess);
}

console.log(crack_(login));
