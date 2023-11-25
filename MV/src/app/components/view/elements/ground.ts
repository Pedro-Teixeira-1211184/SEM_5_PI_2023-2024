import * as THREE from "three";

/*
 * parameters = {
 *  textureUrl: String,
 *  size: Vector2
 * }
 */

export default class Ground {
  textureUrl: string;
  size: THREE.Vector2;
  object: THREE.Mesh;

  constructor(textureUrl: string, size: THREE.Vector2) {
    this.textureUrl = textureUrl;
    this.size = size;

    // Create a texture
    const loader = new THREE.TextureLoader();
    const texture = new THREE.TextureLoader().load(this.textureUrl);
    const geometry = new THREE.PlaneGeometry(this.size.width, this.size.height);
    const material = new THREE.MeshBasicMaterial({color: 0xffffff, map: texture});
    texture.colorSpace = THREE.SRGBColorSpace;
    texture.wrapS = THREE.RepeatWrapping;
    texture.wrapT = THREE.RepeatWrapping;
    texture.repeat.set(this.size.width, this.size.height);
    texture.magFilter = THREE.LinearFilter;
    texture.minFilter = THREE.LinearMipmapLinearFilter;

    this.object = new THREE.Mesh(geometry, material);
    this.object.rotateX(-Math.PI / 2.0);
    this.object.castShadow = false;
    this.object.receiveShadow = true;
  }
}
