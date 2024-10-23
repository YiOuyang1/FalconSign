# FalconSign
High-Speed Hardware Architecture for Falcon Signature Generation Algorithm

The FalconSign project provides a high-speed hardware implementation of the core modules in the Falcon signature algorithm, including the Arithmetic module and the SamplerZ module, which account for 90% of the computational time for digital signature generation. The synthesis and implementation of the modules are carried out on Zynq UltraScale+ ZCU104 platform using Vivado 2022.2.

## Arithmetic module
Used to efficiently perform all the floating-point operations in the Falcon signature generation algorithm. We provide two versions of this module, the Arithmetic module for Sign_Tree and Sign_Dynamic. Sign_Tree uses the pre-computed extended private key format "Falcon Tree" for fast signing, and Sign_Dynamic calculates the "Falcon Tree" dynamically during signature generation.

## SamplerZ module
Used to complete the discrete Gaussian sampling required for Falcon-512 and Falcon-1024.
