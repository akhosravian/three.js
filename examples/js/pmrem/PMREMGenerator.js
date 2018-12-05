/**
 * @author Prashant Sharma / spidersharma03
 * @author Ben Houston / bhouston, https://clara.io
 *
 * To avoid cube map seams, I create an extra pixel around each face. This way when the cube map is
 * sampled by an application later(with a little care by sampling the centre of the texel), the extra 1 border
 *	of pixels makes sure that there is no seams artifacts present. This works perfectly for cubeUV format as
 *	well where the 6 faces can be arranged in any manner whatsoever.
 * Code in the beginning of fragment shader's main function does this job for a given resolution.
 */

THREE.PMREMGenerator = function ( sourceTexture, samplesPerLevel, resolution, sourceResolution ) {

	this.sourceTexture = sourceTexture;
  this.sourceResolution = ( sourceResolution !== undefined ) ? sourceResolution : 512;
	this.resolution = ( resolution !== undefined ) ? resolution : 256; // NODE: 256 is currently hard coded in the glsl code for performance reasons
	this.samplesPerLevel = ( samplesPerLevel !== undefined ) ? samplesPerLevel : 32;

	var monotonicEncoding = ( sourceTexture.encoding === THREE.LinearEncoding ) ||
		( sourceTexture.encoding === THREE.GammaEncoding ) || ( sourceTexture.encoding === THREE.sRGBEncoding );

	this.sourceTexture.minFilter = ( monotonicEncoding ) ? THREE.LinearFilter : THREE.NearestFilter;
	this.sourceTexture.magFilter = ( monotonicEncoding ) ? THREE.LinearFilter : THREE.NearestFilter;
	this.sourceTexture.generateMipmaps = this.sourceTexture.generateMipmaps && monotonicEncoding;

	this.cubeLods = [];

	var size = this.resolution;
	var params = {
		format: this.sourceTexture.format,
		magFilter: this.sourceTexture.magFilter,
		minFilter: this.sourceTexture.minFilter,
		type: this.sourceTexture.type,
		generateMipmaps: this.sourceTexture.generateMipmaps,
		anisotropy: this.sourceTexture.anisotropy,
		encoding: this.sourceTexture.encoding
	 };

	// how many LODs fit in the given CubeUV Texture.
	this.numLods = Math.log( size ) / Math.log( 2 ) - 2; // IE11 doesn't support Math.log2

	for ( var i = 0; i < this.numLods; i ++ ) {

		var renderTarget = new THREE.WebGLRenderTargetCube( size, size, params );
		renderTarget.texture.name = "PMREMGenerator.cube" + i;
		this.cubeLods.push( renderTarget );
		size = Math.max( 16, size / 2 );

	}

	this.camera = new THREE.OrthographicCamera( - 1, 1, 1, - 1, 0.0, 1000 );

	this.shader = this.getShader();
	this.shader.defines[ 'SAMPLES_PER_LEVEL' ] = this.samplesPerLevel;
	this.planeMesh = new THREE.Mesh( new THREE.PlaneBufferGeometry( 2, 2, 0 ), this.shader );
	this.planeMesh.material.side = THREE.DoubleSide;
	this.scene = new THREE.Scene();
	this.scene.add( this.planeMesh );
	this.scene.add( this.camera );

	this.shader.uniforms[ 'envMap' ].value = this.sourceTexture;
	this.shader.envMap = this.sourceTexture;

};

THREE.PMREMGenerator.prototype = {

	constructor: THREE.PMREMGenerator,

	update: function ( renderer ) {

		this.shader.uniforms[ 'envMap' ].value = this.sourceTexture;
		this.shader.envMap = this.sourceTexture;
    this.shader.uniforms[ 'sourceSize' ].value = this.sourceResolution;

		var gammaInput = renderer.gammaInput;
		var gammaOutput = renderer.gammaOutput;
		var toneMapping = renderer.toneMapping;
		var toneMappingExposure = renderer.toneMappingExposure;
		var currentRenderTarget = renderer.getRenderTarget();

		renderer.toneMapping = THREE.LinearToneMapping;
		renderer.toneMappingExposure = 1.0;
		renderer.gammaInput = false;
		renderer.gammaOutput = false;

		for ( var i = 0; i < this.numLods; i ++ ) {

			var r = i / ( this.numLods - 1 );
			this.shader.uniforms[ 'roughness' ].value = r;
      this.shader.uniforms[ 'mapSize' ].value = this.cubeLods[ i ].width;
			this.renderToCubeMapTarget( renderer, this.cubeLods[ i ] );
		}

		renderer.setRenderTarget( currentRenderTarget );
		renderer.toneMapping = toneMapping;
		renderer.toneMappingExposure = toneMappingExposure;
		renderer.gammaInput = gammaInput;
		renderer.gammaOutput = gammaOutput;

	},

	renderToCubeMapTarget: function ( renderer, renderTarget ) {

		for ( var i = 0; i < 6; i ++ ) {

			this.renderToCubeMapTargetFace( renderer, renderTarget, i );

		}

	},

	renderToCubeMapTargetFace: function ( renderer, renderTarget, faceIndex ) {

		renderTarget.activeCubeFace = faceIndex;
		this.shader.uniforms[ 'faceIndex' ].value = faceIndex;
		renderer.render( this.scene, this.camera, renderTarget, true );

	},

	getShader: function () {

		var shaderMaterial = new THREE.ShaderMaterial( {

			defines: {
				"SAMPLES_PER_LEVEL": 20,
			},

			uniforms: {
				"faceIndex": { value: 0 },
				"roughness": { value: 0.5 },
				"mapSize": { value: 0.5 },
        "sourceSize": { value : 256.0 },
				"envMap": { value: null },
				"testColor": { value: new THREE.Vector3( 1, 1, 1 ) },
			},

			vertexShader:
				"varying vec2 vUv;\n\
				void main() {\n\
					vUv = uv;\n\
					gl_Position = projectionMatrix * modelViewMatrix * vec4( position, 1.0 );\n\
				}",

      /* Andrew Khosravian: Hammersley2d functionality from 
       * https://learnopengl.com/PBR/IBL/Specular-IBL
       * more info at 
       * https://blog.selfshadow.com/publications/s2013-shading-course/karis/s2013_pbs_epic_notes_v2.pdf
       */
			fragmentShader:
				"#include <common>\n\
				varying vec2 vUv;\n\
				uniform int faceIndex;\n\
				uniform float roughness;\n\
				uniform samplerCube envMap;\n\
				uniform float mapSize;\n\
				uniform float sourceSize;\n\
				uniform vec3 testColor;\n\
				mat3 MatrixFromVector(vec3 n) {\n\
					float a = 1.0 / (1.0 + n.z);\n\
					float b = -n.x * n.y * a;\n\
					vec3 b1 = vec3(1.0 - n.x * n.x * a, b, -n.x);\n\
					vec3 b2 = vec3(b, 1.0 - n.y * n.y * a, -n.y);\n\
					return mat3(b1, b2, n);\n\
				}\n\
        vec3 GetSampleDirection() {\n\
					vec2 uv = vUv*2.0 - 1.0;\n\
					float offset = -1.0/mapSize;\n\
					const float a = -1.0;\n\
					const float b = 1.0;\n\
					float c = -1.0 + offset;\n\
					float d = 1.0 - offset;\n\
					float bminusa = b - a;\n\
					uv.x = (uv.x - a)/bminusa * d - (uv.x - b)/bminusa * c;\n\
					uv.y = (uv.y - a)/bminusa * d - (uv.y - b)/bminusa * c;\n\
					if (faceIndex==0) {\n\
						return vec3(-1.0, -uv.y, -uv.x);\n\
					} else if (faceIndex==1) {\n\
						return vec3(1.0, -uv.y, uv.x);\n\
					} else if (faceIndex==2) {\n\
						return vec3(-uv.x, 1.0, uv.y);\n\
					} else if (faceIndex==3) {\n\
						return vec3(-uv.x, -1.0, -uv.y);\n\
					} else if (faceIndex==4) {\n\
						return vec3(-uv.x, -uv.y, 1.0);\n\
					} else {\n\
						return vec3(uv.x, -uv.y, -1.0);\n\
					}\n\
        }\n\
        /*\n\
				float RadicalInverse_VdC(uint bits) {\n\
					bits = (bits << 16u) | (bits >> 16u);\n\
					bits = ((bits & 0x55555555u) << 1u) | ((bits & 0xAAAAAAAAu) >> 1u);\n\
					bits = ((bits & 0x33333333u) << 2u) | ((bits & 0xCCCCCCCCu) >> 2u);\n\
					bits = ((bits & 0x0F0F0F0Fu) << 4u) | ((bits & 0xF0F0F0F0u) >> 4u);\n\
					bits = ((bits & 0x00FF00FFu) << 8u) | ((bits & 0xFF00FF00u) >> 8u);\n\
					return float(bits) * 2.3283064365386963e-10; // / 0x100000000\n\
				}\n\
        */\n\
        // slow version using mod since glsl es 2.0 doesn't support bit operations\n\
        float RadicalInverse_VdC(int bits) {\n\
          float result = 0.0;\n\
          float denom = 1.0;\n\
          float invBase = 0.5;\n\
          for (int i = 0; i < 32; ++i) {\n\
            denom = mod(float(bits), 2.0);\n\
            result += denom * invBase;\n\
            invBase *= 0.5;\n\
            bits = int(float(bits) * 0.5);\n\
            if (bits == 0) break;\n\
          }\n\
          return result;\n\
        }\n\
				vec2 Hammersley2d(/*uint*/int i, /*uint*/int N) {\n\
					return vec2(float(i)/float(N), RadicalInverse_VdC(i));\n\
				}\n\
				vec3 ImportanceSampleGGX(vec2 Xi, mat3 vecSpace)\n\
				{\n\
					float a = roughness * roughness;\n\
					float Phi = 2.0 * PI * Xi.x;\n\
					float CosTheta = sqrt( (1.0 - Xi.y) / ( 1.0 + (a*a - 1.0) * Xi.y ) );\n\
					float SinTheta = sqrt( 1.0 - CosTheta * CosTheta );\n\
					return vecSpace * vec3(SinTheta * cos( Phi ), SinTheta * sin( Phi ), CosTheta);\n\
				}\n\
        float DistributionGGX(float NdotH) {\n\
          float rSq = roughness * roughness;\n\
          float NdotHSq = NdotH * NdotH;\n\
          return rSq / pow(NdotHSq * (rSq - 1.0) + 1.0, 2.0);\n\
        }\n\
				vec3 testColorMap() {\n\
					vec3 color;\n\
					if(faceIndex == 0)\n\
						color = vec3(1.0,0.0,0.0);\n\
					else if(faceIndex == 1)\n\
						color = vec3(0.0,1.0,0.0);\n\
					else if(faceIndex == 2)\n\
						color = vec3(0.0,0.0,1.0);\n\
					else if(faceIndex == 3)\n\
						color = vec3(1.0,1.0,0.0);\n\
					else if(faceIndex == 4)\n\
						color = vec3(0.0,1.0,1.0);\n\
					else\n\
						color = vec3(1.0,0.0,1.0);\n\
					color *= ( 1.0 - roughness );\n\
					return color;\n\
				}\n\
				void main() {\n\
					vec3 sampleDirection = GetSampleDirection();\n\
          vec3 N = normalize(sampleDirection);\n\
          vec3 V = N;\n\
					mat3 vecSpace = MatrixFromVector(N);\n\
					vec3 rgbColor = vec3(0.0);\n\
					const int NumSamples = SAMPLES_PER_LEVEL;\n\
          float solidAngleOfTexel = 4.0 * PI / (6.0 * sourceSize * sourceSize);\n\
					float weight = 0.0;\n\
					for( int i = 0; i < NumSamples; i ++ ) {\n\
						vec2 Xi = Hammersley2d(i, NumSamples);\n\
						vec3 H = ImportanceSampleGGX(Xi, vecSpace);\n\
            vec3 L = -reflect(V, H);\n\
						float NdotL = saturate(dot(N, L));\n\
						float NdotH = saturate(dot(N, H));\n\
						float HdotV = saturate(dot(H, V));\n\
            if (NdotL > 0.0) {\n\
              float D = DistributionGGX(NdotH);\n\
              float pdf = (D * NdotH / (4.0 * HdotV));\n\
              float solidAngleOfSample = 1.0 / (float(SAMPLES_PER_LEVEL) * pdf);\n\
              float mipLevel = roughness == 0.0 ? 0.0 : 0.5 * log2(solidAngleOfSample / solidAngleOfTexel);\n\
              rgbColor.rgb += envMapTexelToLinear(textureCube(envMap, L, mipLevel)).rgb * NdotL;\n\
              weight += NdotL;\n\
            }\n\
					}\n\
          if (weight > 0.0) {\n\
					  rgbColor /= weight;\n\
          }\n\
					//rgbColor = testColorMap();\n\
					gl_FragColor = linearToOutputTexel( vec4( rgbColor, 1.0 ) );\n\
				}",

			blending: THREE.NoBlending

		} );

		shaderMaterial.type = 'PMREMGenerator';

		return shaderMaterial;

	},

	dispose: function () {

		for ( var i = 0, l = this.cubeLods.length; i < l; i ++ ) {

			this.cubeLods[ i ].dispose();

		}

		this.planeMesh.geometry.dispose();
		this.planeMesh.material.dispose();

	}

};
