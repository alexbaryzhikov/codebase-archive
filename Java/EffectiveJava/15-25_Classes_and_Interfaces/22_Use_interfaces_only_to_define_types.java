
// Constant interface antipattern - do not use!
public interface PhysicalConstants {

  // Avogadro's number (1/mol)
  static final double AVOGADROS_NUMBER = 6.02214199e23;

  // Boltzmann constant (J/K)
  static final double BOLTZMANN_CONSTANT = 1.3806503e-23;

  // Mass of the electron (kg)
  static final double ELECTRON_MASS = 9.10938188e-31;
}

// NOTE The constant interface pattern is a poor use of interfaces.

// Constant utility class
package com.effectivejava.science;

public class PhysicalConstants {

  public static final double AVOGADROS_NUMBER = 6.02214199e23;
  public static final double BOLTZMANN_CONSTANT = 1.3806503e-23;
  public static final double ELECTRON_MASS = 9.10938188e-31;

  private PhysicalConstants() { }  // Prevents instantiation
}

// Use of static import to avoid qualifying constants
import static com.effectivejava.science.PhysicalConstants.*;

public class Test {

  double atoms(double mols) {
    return AVOGADROS_NUMBER * mols;
  }

  ...  // Many more uses of PhysicalConstants justify static import
}
