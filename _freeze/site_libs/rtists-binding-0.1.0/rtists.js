HTMLWidgets.widget({

  name: 'rtists',

  type: 'output',

  factory: function(el, width, height) {

    const renderer = new THREE.WebGLRenderer({ antialias: true });
    renderer.setSize(width, height);
    el.appendChild(renderer.domElement);

    const scene  = new THREE.Scene();
    const camera = new THREE.PerspectiveCamera(55, width / height, 0.1, 10000);

    // ---- controls ---------------------------------------------------------
    const controls = new THREE.OrbitControls(camera, renderer.domElement);

    // ---- lighting -----------------------------------------------------------
    scene.add(new THREE.AmbientLight(0xffffff, 0.6));
    const dir = new THREE.DirectionalLight(0xffffff, 0.6);
    dir.position.set(1, 1, 1);
    scene.add(dir);

    // ---- spatial reference (axes at data origin, in tract units) -----------
    const axes = new THREE.AxesHelper(20);
    scene.add(axes);

    let mesh        = null;
    let cameraFrame = null;   // cached radius/center for resize
    let rafId       = null;   // requestAnimationFrame handle

    // three.js only draws when renderer.render() is called. Drive it with a
    // rAF loop so OrbitControls animate smoothly; cancel on widget removal.
    function startLoop() {
      function frame() {
        controls.update();
        renderer.render(scene, camera);
        rafId = requestAnimationFrame(frame);
      }
      cancelAnimationFrame(rafId);
      rafId = requestAnimationFrame(frame);
    }
    startLoop();

    function frameCamera(positions) {
      const n = positions.length;
      let minX = Infinity, minY = Infinity, minZ = Infinity;
      let maxX = -Infinity, maxY = -Infinity, maxZ = -Infinity;
      for (let i = 0; i < n; i += 3) {
        const x = positions[i], y = positions[i+1], z = positions[i+2];
        if (x < minX) minX = x; if (x > maxX) maxX = x;
        if (y < minY) minY = y; if (y > maxY) maxY = y;
        if (z < minZ) minZ = z; if (z > maxZ) maxZ = z;
      }
      const center = new THREE.Vector3(
        (minX + maxX) / 2, (minY + maxY) / 2, (minZ + maxZ) / 2);
      const radius = Math.max(maxX - minX, maxY - minY, maxZ - minZ) * 0.5;

      cameraFrame = { center, radius: Math.max(radius, 1e-3) };
      placeCamera();
    }

    function placeCamera() {
      if (!cameraFrame) return;
      const { center, radius } = cameraFrame;
      controls.target.copy(center);
      camera.near = radius / 1000;
      camera.far  = radius * 100;
      // camera looks at center from a fixed diagonal offset
      camera.position.copy(center).add(
        new THREE.Vector3(1, 1, 1).normalize().multiplyScalar(radius * 1.6)
      );
      camera.updateProjectionMatrix();
      controls.update();
    }

    // Dispose a mesh or a Group of meshes, releasing GPU geometry/material.
    function disposeObject(obj) {
      if (!obj) return;
      if (obj.isGroup) {
        obj.traverse(function(child) {
          if (child.isMesh) {
            child.geometry.dispose();
            if (Array.isArray(child.material)) {
              child.material.forEach(function(m) { m.dispose(); });
            } else if (child.material) {
              child.material.dispose();
            }
          }
        });
      } else if (obj.geometry) {
        obj.geometry.dispose();
        if (obj.material) obj.material.dispose();
      }
    }

    // Build one tube per streamline. Each streamline (a polyline of points) is
    // turned into a TubeGeometry around a CatmullRomCurve3 with radius
    // `thickness` (world units). Per-vertex colours are sampled from the
    // matching streamline's point colours (proportional across the tube rings).
    // One material per tube + `transparent:true` + `depthWrite:false` lets
    // three.js depth-sort tubes back-to-front, giving object-level
    // depth-sorted transparency (the closest WebGL gets without per-fragment
    // sorting). Returns a THREE.Group.
    function buildTubes(x, transparent) {
      const group = new THREE.Group();
      const tubular = Math.min(64, 8);
      const radial  = 6;
      const pos = x.positions;
      const col = x.colors;
      const off = x.tubeOffsets;

      for (let t = 0; t < off.length - 1; t++) {
        const a = off[t], b = off[t + 1];
        const n = b - a;
        if (n < 2) continue;
        const pts = [];
        for (let i = a; i < b; i++) pts.push(new THREE.Vector3(pos[3*i], pos[3*i+1], pos[3*i+2]));
        const curve = new THREE.CatmullRomCurve3(pts);
        const geo = new THREE.TubeGeometry(curve, tubular * n, x.thickness, radial, false);
        // per-vertex RGB: map the ring index onto the streamline's point colours
        const vCount = geo.attributes.position.count;
        const ringPts = tubular * n + 1;
        for (let v = 0; v < vCount; v++) {
          const ring  = Math.floor(v / (radial + 1));
          const pIdx  = Math.min(n - 1, Math.round(ring / ringPts * (n - 1)));
          const ci    = 3 * (a + pIdx);
          geo.setColorAt(v, new THREE.Color(col[ci], col[ci+1], col[ci+2]));
        }
        const mat = new THREE.MeshBasicMaterial({
          vertexColors: true,
          transparent:  transparent,
          opacity:      x.opacity,
          depthWrite:   false
        });
        const tube = new THREE.Mesh(geo, mat);
        group.add(tube);
      }
      return group;
    }

    return {

      resize: function(width, height) {
        camera.aspect = width / height;
        camera.updateProjectionMatrix();
        renderer.setSize(width, height);
      },

      destroy: function() {
        cancelAnimationFrame(rafId);
        if (mesh) {
          scene.remove(mesh);
          disposeObject(mesh);
        }
        renderer.dispose();
        controls.dispose();
        if (renderer.domElement.parentNode) {
          renderer.domElement.parentNode.removeChild(renderer.domElement);
        }
      },

      renderValue: function(x) {
        scene.background = new THREE.Color(x.bg);

        if (mesh) {
          scene.remove(mesh);
          disposeObject(mesh);
          mesh = null;
          cameraFrame = null;
        }

        const transparent = x.opacity < 1;
        let framePositions = null;

        // ---- tube mode (real world-space thickness) ---------------------------
        // When x.thickness > 0, each streamline is a TubeGeometry built around
        // its polyline, coloured per-point by the provided RGB. This gives true
        // 3D fibre thickness (unlike LineBasicMaterial, whose linewidth is
        // capped at 1). x.tubeOffsets marks streamline boundaries (cumulative
        // point index, length n_streamlines + 1).
        if (x.thickness > 0 && x.tubeOffsets) {
          mesh = buildTubes(x, transparent);
          framePositions = x.positions;   // per-point coords cover all tubes

        // ---- line mode (default, fast) ---------------------------------------
        } else {
          const geometry = new THREE.BufferGeometry();
          geometry.setAttribute(
            'position',
            new THREE.BufferAttribute(new Float32Array(x.positions), 3)
          );
          geometry.setAttribute(
            'color',
            new THREE.BufferAttribute(new Float32Array(x.colors), 4)
          );

          const material = new THREE.LineBasicMaterial({
            vertexColors: true,
            linewidth:    x.linewidth,
            transparent:  transparent,
            opacity:      x.opacity
          });

          mesh = new THREE.LineSegments(geometry, material);
          framePositions = geometry.attributes.position.array;
        }

        scene.add(mesh);
        frameCamera(framePositions);
      },

      sizing: {},
      instanceMethods: {}
    };
  }
});
