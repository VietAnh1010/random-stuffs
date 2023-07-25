  const lanczosCoefficients = [
    0.99999999999980993, 676.5203681218851, -1259.1392167224028,
    771.32342877765313, -176.61502916214059, 12.507343278686905,
    -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7
  ];

function complexGamma(z) {
  // Check if z is a real number
  const g = 7;
  const complexOne = { re: 1, im: 0 };
  if (Math.abs(z.im) < Number.EPSILON) {
    return realGamma(z.re);
  }

  // Define the Lanczos coefficients


  // Perform the Lanczos approximation
  function lanczosApproximation(z) {
    if (z < 0.5) {
      return Math.PI / (Math.sin(Math.PI * z) * lanczosApproximation(1 - z));
    }

    z -= 1;

    let x = lanczosCoefficients[0];
    for (let i = 1; i < g + 2; i++) {
      x += lanczosCoefficients[i] / (z + i);
    }

    const t = z + g + 0.5;

    return Math.sqrt(2 * Math.PI) * Math.pow(t, z + 0.5) * Math.exp(-t) * x;
  }

  // Calculate the real Gamma function
  function realGamma(x) {
    if (x <= 0) {
      throw new Error("Gamma function is not defined for non-positive integers.");
    }

    if (x === 1) {
      return 1;
    }

    if (x < 1) {
      return realGamma(x + 1) / x;
    }

    return lanczosApproximation(x - 1);
  }

  // Perform the complex Gamma function
  const realPart = realGamma(z.re) * Math.cos(-Math.PI * z.re) * Math.cosh(Math.PI * z.im);
  const imagPart = -realGamma(z.re) * Math.sin(-Math.PI * z.re) * Math.sinh(Math.PI * z.im);

  return { re: realPart, im: imagPart };
}

// Example usage
let z = { re: 2, im: 3 };
let result = complexGamma(z);
console.log(result);

z = { re: 3, im: 0 };
result = complexGamma(z);
console.log(result);
